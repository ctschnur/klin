;;; klin-bibtex.el --- Bibtex file editing functionality for klin workflow  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <>
;; Keywords: tools, bib, tex, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; for consecutive processing of individual bibtex files assoc. to single pdfs

;; association list between bibfiles (paths) and frames where they are opened
;; while batch processing them (actually, only ever one frame of that
;; association list is not nil)

(require 'bibtex)
(require 'org-ref-utils)
(require 'org-ref-core)
(require 'klin-pdf-frames)
(require 'org-ref-isbn)
(require 'helm-mode)
(require 'klin-utils)
(require 'parsebib)

(defvar pdf-bib-window nil)
(defvar pdf-file-path-queue '())
(defvar current-pdf-file-path nil)

;; -------- macro
(defmacro with-bib-file-buffer (bib-file-path &rest body)
  "Run BODY in a fully parsed bibfile buffer of the file BIB-FILE-PATH, without opening that buffer visually."
  `(progn
     (with-temp-buffer
       (insert-file-contents ,bib-file-path)
       (bibtex-mode)
       (bibtex-set-dialect (parsebib-find-bibtex-dialect)
                           t)
       (bibtex-parse-keys)
       (goto-char (point-min))
       ,@body)))
;; --------

;; -------- basic things
(defun klin-bibtex-reparse-bib-buffer ()
  "Re-parses the bib buffer."
    (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
    (bibtex-parse-keys))
;; --------

;; -------- tools for higher-level interactive functions

(defun klin-bibtex-next-entry (&optional n)
  "Jump to the beginning of the next (N th next) bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
                     (bibtex-beginning-of-entry)))
    (forward-char)
    (klin-bibtex-next-entry))
  ;; search forward for an entry
  (bibtex-set-dialect (parsebib-find-bibtex-dialect)
                      t) ; initialize bibtex-entry-head
  (when (re-search-forward bibtex-entry-head
                           nil
                           t
                           (and (numberp n)
                                n))
    ;; go to beginning of the entry
    (bibtex-beginning-of-entry)))

(defun klin-bibtex-previous-entry (&optional n)
  "Jump to beginning of the (Nth) previous bibtex entry.
N is a prefix argument."
  (interactive "P")
  (bibtex-beginning-of-entry)
  (when (re-search-backward bibtex-entry-head
                            nil
                            t
                            (and (numberp n)
                                 n))
    (bibtex-beginning-of-entry)))

(defun klin-bibtex-get-pdf-filepath-for-bibtex-entry (&optional key)
  "Try to find the pdf associated with the bib file.
KEY can be provided, but if not it will try to figure
the pdf filename out by the buffer file name."
  (interactive)
  ;; re-parse the bib buffer
  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
  (bibtex-parse-keys)

  (helm-read-file-name
   (concat "what is the corresponding pdf?"
           (if (or (string= "" key) (not key))
               "(key: not set)"
             (concat "(key: " key " )"))
           ": ")
   :initial-input
   (let* ((filepath-field (bibtex-get-field-from-entry-under-cursor
                           "filepath" (current-buffer)))
          (pdf-filepath-guess-from-bibfile-path (let* ((guess
                                                        ;; it's the folder's name + .pdf
                                                        (concat (file-name-directory (directory-file-name (file-name-directory (directory-file-name (buffer-file-name)))))
                                                                (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))) ".pdf")))
                                                  ;; check if the pdf actually exists
                                                  (if (file-exists-p guess)
                                                      guess))))

     (if (klin-bibtex-field-value-set-p filepath-field)
         (klin-utils-get-reduced-file-path (expand-file-name filepath-field))
       (if pdf-filepath-guess-from-bibfile-path
           (klin-utils-get-reduced-file-path (expand-file-name pdf-filepath-guess-from-bibfile-path)))))))

;; (defun klin-bibtex-filename-to-pdf-filename (bibtex-filename)
;;   "Convert BIBTEX-FILENAME to the standard associated pdf filename.
;; By convention, here, a pdf file example.pdf should have a standard
;; associated bibtex file .example.pdf.bib in the same folder."
;;   (let* ((filename bibtex-filename))
;;     (string-match "^\.\\(+?.*\\)\.bib$" filename)
;;     (match-string 1 filename)))

(defun klin-pdf-filename-to-bibtex-filename (pdf-filename)
  "Convert PDF-FILENAME to the standard associated bib filename."
  (concat "." pdf-filename ".bib"))

(defun klin-pdf-filepath-to-bibtex-filepath (pdf-filepath)
  "Convert PDF-FILEPATH (not pdf-filename) to a bib file's path."
  (let* ((pdf-filename (file-name-nondirectory pdf-filepath))
         (bibtex-filename (klin-pdf-filename-to-bibtex-filename pdf-filename))
         (bibtex-filepath (concat (file-name-directory pdf-filepath)
                                  bibtex-filename)))
    bibtex-filepath))

(defun klin-ask-pdf-offset-number (num)
  "Prompt user to enter the page offset number NUM of a pdf.
This may be called inside an opened pdf, to e.g. manually add a
page-offset field to a bibtex entry."
  (interactive (list (read-number "pdf page offset number (\"which pdf page is book page 0?\"): ")))
  (message "number is %s."
           (number-to-string num))
  num)

(defun bibtex-get-field-from-entry-under-cursor
    (field &optional bibfile-buffer)
  "Get FIELD's value from bib entry in BIBFILE-BUFFER under cursor."
  (unless bibfile-buffer
    (setq bibfile-buffer (current-buffer)))
  (with-current-buffer bibfile-buffer
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (field-value (org-ref-reftex-get-bib-field field entry)))
        field-value))))

(defun klin-bibtex-get-field (field &optional key)
  "From within a bibtex buffer, return key value of FIELD of a bib entry with KEY in BIBFILE-PATH.
Unless KEY is given, use the entry under cursor.  Unless BIBFILE-PATH
is given, search in the current bib buffer."
  (klin-bibtex-reparse-bib-buffer)
  (let* (entry
         key-under-cursor)

    (unless key
      ;; get the key of the entry under cursor
      (setq key-under-cursor (bibtex-get-field-from-entry-under-cursor "=key="
                                                                       (current-buffer)))
      (when key-under-cursor
        (setq key key-under-cursor))
      ;; if just one, get it's key
      (if (= 1 (length (bibtex-global-key-alist)))
          (setq key (car (nth 0
                              (bibtex-global-key-alist))))))

    ;; find the one you are looking for, after parsing keys
    (if (bibtex-search-entry key nil 0)
        (progn
          (setq entry (bibtex-parse-entry)) ; sets cursor at the end of entry's last field
          (let ((field-value (org-ref-reftex-get-bib-field field entry)))
            (if (>= (length field-value) 1)
                field-value
              (message (concat "no field " field " found in " (buffer-file-name)
                               " -> " key))
              nil))) ;; sets cursor at the beginning of the entry's line
      (message (concat "no key " key " found in " (buffer-file-name)))
      nil)))

;; ---------- lower complexity interactive functions

(require 'klin-optional)

(defun klin-bibtex-open-pdf-from-bibtex (&optional bibtexkey page)
  "From within a bibtex buffer, open BIBTEXKEY's pdf file on PAGE, respecting page offset."
  (interactive)

  (unless bibtexkey
    (setq bibtexkey (klin-bibtex-get-field "=key=")))

  (let* ((result (klin-bibtex-get-field "file-page-offset"
                                        bibtexkey))
         (file-page-offset (if (eq 'string (type-of result))
                               (string-to-number result)
                             0))
         (filepath (klin-bibtex-get-field "filepath" bibtexkey))
         (page (- (+ (if page page 0) file-page-offset)
                  ;; 1
                  0)))

    (if my-open-the-annotated-version-first
        ;; check if there is an annotated version
        (let* ((annotated-pdf-filepath (concat (file-name-nondirectory filepath)
                                               (my-get-freehand-note-annotating-filename (file-name-base filepath)))))
          (if (file-exists-p annotated-pdf-filepath)
              (progn
                (setq filepath annotated-pdf-filepath)
                (message "Showing the *annotated* version")))))

    (open-pdf-document-other-frame-or-window filepath page nil -1)))

(defun klin-bibtex-grab-template-isbn (&optional isbn buf-to-return-to)
  "Fill an empty bib file with ISBN template an switch to buffer PDF-BUF-TO-RETURN-TO."
  (interactive)
  (unless isbn
    (setq isbn (read-string "ISBN? (return to continue):"))
    (if buf-to-return-to
      (other-buffer buf-to-return-to)))
  (isbn-to-bibtex isbn (buffer-file-name (current-buffer))))

(defun get-bibtex-from-doi (doi)
 "Get a BibTeX entry from the DOI; found here: https://www.anghyflawn.net/blog/2014/emacs-give-a-doi-get-a-bibtex-entry/"
 (interactive "MDOI: ")
 (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
   (with-current-buffer
     (url-retrieve-synchronously
       (format "http://dx.doi.org/%s"
       	(replace-regexp-in-string "http://dx.doi.org/" "" doi)))
     (switch-to-buffer (current-buffer))
     (goto-char (point-max))
     (setq bibtex-entry
     	  (buffer-substring
          	(string-match "@" (buffer-string))
              (point)))
     (kill-buffer (current-buffer))))
 (insert (decode-coding-string bibtex-entry 'utf-8))
 (bibtex-fill-entry))

;; ----------

(defun klin-bibtex-cursor-on-entry-head-line-beginning-p ()
  "Within bibtex buffer, checks if cursor is on an entry or not."
  (save-excursion
    (klin-bibtex-reparse-bib-buffer)
    ;; returns nil if not on an entry,
    ;; otherwise it returns a list of properties
    (bibtex-parse-entry)))

;; --------- open all linked pdfs in a bibtex buffer

(defun generate-them (bibfile-buffer)
  "Within a BIBFILE-BUFFER, get all referenced pdf files.
Return a list (description filepath)."
  (with-current-buffer bibfile-buffer
    (message "generate-them %s" (buffer-file-name))
    (print (current-buffer))
    (save-excursion
      (klin-bibtex-reparse-bib-buffer)
      (goto-char (point-min))
      (mapcar (lambda (key-tuple)
                (let* ((key (nth 0 key-tuple))
                       ;; (title (klin-bibtex-get-field "title" key))
                       (filepath (klin-bibtex-get-field "filepath" key)))
                  (list (concat key
                                ;; " | "
                                ;; title
                                " | "
                                filepath
                                )
                        filepath)))
              ;; generate list of all entries
              (bibtex-global-key-alist)))))

(require 'klin-tabs)

(defun klin-bibtex-open-pdfs-in-bibtex-file ()
  "Within bibtex buffer, run this to open a list of pdf files referenced therein."
  (interactive)
  (let* ((bibfile-buffer (current-buffer)))
    (helm :sources `(((name . "PDF filepaths referenced in: ")
                      ;; (candidates . klin-bibtex-get-all-referenced-pdf-filepaths-in-bibtex-file)
                      ;; (candidates . mylistofcands)
                      (candidates . (lambda ()
                                      (generate-them ,bibfile-buffer)))
                      (action . (lambda (candidate)
                                  ;; (message "yes: %s" candidate)
                                  ;; (print (helm-marked-candidates))
                                  ;; (add-to-list 'klin-bibtex-tmplistofpdfstoopen
                                  ;;              (car candidate) t)
                                  (let* ((pdf-filepaths-to-open
                                          (mapcar (lambda (elem)
                                                    (car elem))
                                                  (helm-marked-candidates))))
                                    (klin-tabs-open-pdfs-in-new-frame pdf-filepaths-to-open)))))))))
;; ---------

;; ---------- higher complexity interactive functions

(defun klin-bibtex-compare-entry-to-original-bibfile ()
  "Open up the original bibfile, if it's to be found.
Then, it is left up to the user to compare or sync up the entrys."
  (interactive)
  (let* ((reduced-pdf-filepath (bibtex-get-field-from-entry-under-cursor "filepath"))
         (current-key (bibtex-get-field-from-entry-under-cursor "=key="))
         (original-bibfile-path
          (let* ((bib-filepath-from-pdf-filepath-field
                  (klin-utils-pdf-get-self-bib-file-path
                   (expand-file-name reduced-pdf-filepath)))
                 (suggested-initial-input (concat pdf-library-dir " .bib$ ")))
            (if (file-exists-p bib-filepath-from-pdf-filepath-field)
                (setq suggested-initial-input
                      bib-filepath-from-pdf-filepath-field))
            (helm-read-file-name
             "select bib file to compare (e.g. original bibfile): "
             :initial-input suggested-initial-input))))
    ;; then, open it up in another (maybe split) window
    (find-file-other-window original-bibfile-path)

    (let* (reduced-pdf-filepath-search-position
           bibtex-find-entry-search-position)
      (goto-char (point-min))
      (save-excursion
        (setq reduced-pdf-filepath-search-position
              (re-search-forward reduced-pdf-filepath)))
      (save-excursion
        (setq bibtex-find-entry-search-position
              (bibtex-find-entry current-key)))
      (if reduced-pdf-filepath-search-position
          (progn
            (message "found matching filepath")
            (goto-char reduced-pdf-filepath-search-position)))
      (if bibtex-find-entry-search-position
          (progn
            (message "found matching key")
            (goto-char bibtex-find-entry-search-position))))))

;; ---------- integration of entrys into other bibfiles

(defun klin-bibtex-integrate-entry-into-collective-bibfile (candidate)
  "Integrate entry under cursor into collective bibfile CANDIDATE."
  (let* ((coll-bibtex-filepath (car candidate)))
                      ;; go into coll-bibtex-filepath's file
                      ;; anda file
                      ;; copy an entry in the one buffer to a kill-ring
                      (bibtex-copy-entry-as-kill)
                      (when (find-file-other-window coll-bibtex-filepath)
                        (goto-char (point-max))
                        (insert "\n\n")
                        (insert (car bibtex-entry-kill-ring))
                        (bibtex-validate))))

(defun klin-bibtex-get-collective-bibtex-files ()
  "Get collective bibtex files in the current context.
Search for an open org file and wether it has a bib file
associated to it."
  (mapcar (lambda (tup) (list (concat (nth 0 tup)
                                      " -> "
                                      (nth 1 tup))
                              (nth 1 tup)))
          (delq nil (delete-dups
                     (reverse
                      (-flatten-n 1 (remove-if (lambda (x)
                                                 (eq x nil))
                                               (mapcar (lambda (buffer)
                                                         (with-current-buffer buffer
                                                           (when (buffer-file-name)
                                                             (mapcar (lambda (bibfilepath)
                                                                       (list (buffer-file-name)
                                                                             bibfilepath))
                                                                     (klin-org-org-ref-find-bibliography-fullfilenames)))))
                                                       (remove-if (lambda (x)
                                                                    (eq x nil))
                                                                  (mapcar (lambda (buffer)
                                                                            (with-current-buffer buffer
                                                                              (if (eq major-mode 'org-mode)
                                                                                  (current-buffer))))
                                                                          (buffer-list)))))))))))

(defvar helm-source-klin-bibtex-entry-into-collective
      '((name . "integrate bibtex entry into collective bibtex file:")
        (candidates . klin-bibtex-get-collective-bibtex-files)
        (action . (lambda (candidate)
                    (klin-bibtex-integrate-entry-into-collective-bibfile candidate)))))

(defun klin-bibtex-integrate-bib-entry-into-collective ()
  "Integrate entry under cursor into a collective bibtex file."
  (interactive)
  (helm :sources '(helm-source-klin-bibtex-entry-into-collective)))

;; ---------

(defun jlp/add-to-list-multiple (list to-add)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item))
  list)

;; --------- creating/editing self-bibfiles for pdfs

(defun klin-bibtex-set-bibfiles-for-pdfs (&optional filepaths)
  "Mark some PDFs with helm, then add them to global list."
  (interactive)
  (unless filepaths
    (setq filepaths (helm-read-file-name "Set bibfiles for marked pdfs: "
                                         :initial-input (concat pdf-library-dir " .pdf$")
                                         :marked-candidates t)))
  ;; globally add the selected pdfs
  (jlp/add-to-list-multiple 'pdf-file-path-queue filepaths)
  ;; (setq pdf-file-path-queue '())
  ;; start with visiting the first one
  (klin-visit-pdf-self-bibtex-file (car pdf-file-path-queue)))

(defun klin-bibtex-edit-interactively (intended-bibfile-path)
  "Visit INTENDED-BIBFILE-PATH and displays help buffer."
  (interactive)
  (let* ()
    (find-file intended-bibfile-path)
    ;; (with-help-window (help-buffer)
    ;;   (princ (concat "TODO: \n"
    ;;                  "- download bibtex entry form isbn (if possible)\n"
    ;;                  "  - get isbn template (if possible) (copy)\n"
    ;;                  "  - ask for isbn (paste) \n"
    ;;                  "- set file path and file page offset fields of entry\n"
    ;;                  "- integrate entry into collective bibtex file (optional)")))
    ))

(defun klin-visit-pdf-self-bibtex-file (&optional pdfpath)
  "For a pdf at PDFPATH, start making a bibtex entry."
  (interactive)
  (let* ((bibfilepath (klin-utils-pdf-get-self-bib-file-path pdfpath)))
    ;; check if pdf actually exists
    (unless (file-exists-p pdfpath)
      (unless (yes-or-no-p "PDF file doesn't exist, continue anyway?")
        (error "PDF file doesn't exist, chose to quit")))

    ;; this appends to a file (and creates one if there is none)
    (unless (file-exists-p (file-name-directory bibfilepath))
      (klin-utils-ask-to-create-dir (file-name-directory bibfilepath)))

    (with-temp-buffer (write-region "" nil bibfilepath 'append))

    ;; set global state variable
    (setq current-pdf-file-path pdfpath)

    (klin-bibtex-edit-interactively bibfilepath)
    bibfilepath))

(defun klin-bibtex-process-next-pdf ()
  "Process the next PDF in queue."
  (interactive)
  (let* ((next-pdf-filepath (nth
                             (+ 1
                                (cl-position current-pdf-file-path pdf-file-path-queue))
                             pdf-file-path-queue)))
    (klin-visit-pdf-self-bibtex-file next-pdf-filepath)))

(defun klin-bibtex-process-previous-pdf ()
  "Process the previous PDF in queue."
  (interactive)
  (let* ((next-pdf-filepath (nth
                             (- (cl-position current-pdf-file-path pdf-file-path-queue)
                                1)
                             pdf-file-path-queue)))
    (klin-visit-pdf-self-bibtex-file next-pdf-filepath)))

(defun klin-bibtex-process-finish-current-pdf ()
  (interactive)
  ;; check if it's a file
  (if (buffer-file-name)
      (let* ( ;; see klin-bibtex.el
             (orig-pos (cl-position current-pdf-file-path pdf-file-path-queue)))
        (kill-buffer (current-buffer))
        ;; remove from list
        (setq pdf-file-path-queue (delete current-pdf-file-path pdf-file-path-queue))
        (let* ((in-place-pdf-filepath (let* ((orig-pos-elem-now (nth orig-pos pdf-file-path-queue))
                                             (orig-pos-prev-elem (nth (- orig-pos 1)
                                                                      pdf-file-path-queue)))
                                        (if orig-pos-elem-now
                                            orig-pos-elem-now
                                          (if orig-pos-prev-elem
                                              orig-pos-prev-elem
                                            (message "No stuff left. You can quit.")
                                            nil)))))
          (if in-place-pdf-filepath
              ;; open it
              (klin-visit-pdf-self-bibtex-file in-place-pdf-filepath))))))

(defun klin-bibtex-process-open-current-pdf ()
  (interactive)
  (split-window)
  (find-file-existing current-pdf-file-path))

(defun klin-bibtex-process-clean-whole-list ()
  (interactive)
  (if (y-or-n-p (format "Clear out entire list?"))
      (progn
        (setq current-pdf-file-path nil)
        (setq pdf-file-path-queue '()))))

;; ---------

;; --------- fixing entry fields

(defun klin-bibtex-entry-open-pdf ()
  "Assist the filling in of pdf-file related fields in .bib entry."
  (interactive)

  ;; re-parse the bib buffer
  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
  (bibtex-parse-keys)

  ;; if it's empty, try to find the pdf from the file name first,
  ;; in order to get to the isbn, which enables you to get the
  ;; title, author, year, publisher data from the internet
  (let* (pdf-filepath)
    (if (eq (length (bibtex-global-key-alist)) 0)
        (setq pdf-filepath (klin-bibtex-get-pdf-filepath-for-bibtex-entry)))
    ;; now that we have the file path, open the pdf to find the isbn
    (message "We'll open up the pdf now. You could try to find the isbn.")
    (sleep-for 2)
    ;; (split-window)
    ;; (other-window 1)
    (find-file pdf-filepath)
    (with-help-window (help-buffer)
        (princ (concat "TODO: \n"
                       "- search the isbn\n"
                       "- copy it\n"
                       "- return to bib buffer and ask for isbn (key command)\n")))
    (message "Please search and highlight the ISBN.")
    ))

(defun klin-bibtex-entry-fix-filepath-file-page-offset ()
  "Assists the user in setting pdf-related data fields in bibtex file."
  (interactive)
  (let* ((filepath-field-str
         (bibtex-get-field-from-entry-under-cursor
          "filepath" (current-buffer)))
         (file-page-offset-field-str
          (bibtex-get-field-from-entry-under-cursor
           "file-page-offset" (current-buffer)))
         )
    (unless (or (not (string= "" filepath-field-str)) (not filepath-field-str))
      (klin-bibtex-entry-fix-filepath-field))
    (unless (or (not (string= "" file-page-offset-field-str))
                (not file-page-offset-field-str))
      (klin-bibtex-entry-fix-file-page-offset))
    )
  )

(defun klin-bibtex-entry-fix-filepath-field (&optional key)
  "Set filepath field of KEY's bib entry.
If KEY is not set, edit entry under cursor."
  (interactive)
  (let* (filepath)
    (unless key
      (setq key (bibtex-get-field-from-entry-under-cursor "=key="
                                                          (current-buffer))))
    (setq filepath (klin-bibtex-get-pdf-filepath-for-bibtex-entry key))
    (bibtex-set-field "filepath"
                      (format "\"%s\""
                              (klin-utils-get-reduced-file-path filepath))))
  (bibtex-clean-entry)
  ;; (bibtex-fill-entry)
  )

(defun klin-bibtex-field-value-set-p (result)
  (let* (;; (result (klin-bibtex-get-field field))
         )
    (and (not (string-equal result ""))
         result)))

(defun klin-bibtex-entry-fix-file-page-offset (&optional key open-seperate-frame)
  "Asks for the KEY's entry's pdf file page offset, with the choice of opening the pdf."
  (interactive)
  (let* ((bib-buffer (current-buffer))
         ;; (bib-window (selected-window))
         ;; pdf-buffer
         pdf-frame
         (pdf-filepath (klin-bibtex-get-field "filepath"))
         offset-number)
    (unless key
      (setq key (bibtex-get-field-from-entry-under-cursor
                 "=key=" bib-buffer)))

    (if (not (klin-bibtex-field-value-set-p pdf-filepath))
        (setq pdf-filepath
              (helm-read-file-name
               (concat "pdf filepath for " key ": ")
               ;; suggest (and search) the pdf path with helm
               :initial-input (let* ((standard-folder-path (expand-file-name pdf-library-dir)))
                                (concat (if (file-exists-p standard-folder-path)
                                            standard-folder-path
                                          "")
                                        " .pdf$ ")))))

    ;; (setq pdf-frame (open-pdf-document-new-frame (expand-file-name pdf-filepath) 1))

    ;; split the window to open the pdf
    (let* ((frame-and-window
            (open-pdf-document-in-selected-frame (expand-file-name pdf-filepath) 1 1))
           (pdf-frame (car frame-and-window))
           (pdf-window (cadr frame-and-window)))
      (with-selected-frame pdf-frame
        (setq offset-number (call-interactively 'klin-ask-pdf-offset-number))))

    ;; (delete-frame pdf-frame)
    (pop-to-buffer bib-buffer)
    (bibtex-set-field "file-page-offset"
                      (number-to-string offset-number)))
  (bibtex-clean-entry)
  ;; (bibtex-fill-entry)
  )

;; -----------

;; ----------- 3rd party tools

(defun jmax-bibtex-get-fields ()
  "Get a list of fields in a bibtex entry."
  (bibtex-beginning-of-entry)
  (remove "=type="
          (remove "=key="
                  (mapcar 'car (bibtex-parse-entry)))))

(defun jmax-bibtex-jump-to-field (field)
  "Jump to FIELD in the current bibtex entry."
  (interactive
   (list
    (ido-completing-read "Field: " (jmax-bibtex-get-fields))))
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (when
        ;; fields start with spaces, a field name, possibly more
        ;; spaces, then =
        (re-search-forward (format "^\\s-*%s\\s-*=" field) nil t))))

;; ----------

(provide 'klin-bibtex)
;;; klin-bibtex.el ends here
