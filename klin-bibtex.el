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

(defvar bib-processing-bibfiles-frames nil)

(defvar bib-processing-currently-processing nil)

(defun isbn-to-bibtex-ottobib-insert (isbn)
  "Insert ottobib bib entry suggestion for ISBN."
  (interactive "sISBN: ")
  ;; (unless isbn (setq isbn "0195117972"))
  (let* ((tmpfilename (make-temp-file "ottobib")
                      ;; "/home/chris/Dropbox/stuff/1Book/elisp/bar"
                      )
         (bibtex-text
          (let* (beginning
                 ending)
            (url-copy-file (concat "https://www.ottobib.com/isbn/ " isbn "/bibtex") tmpfilename t)
            (with-temp-buffer
              (insert-file-contents tmpfilename)
              (goto-char (point-min))
              (setq beginning (re-search-forward "textarea.+?>"))
              (setq ending (progn (re-search-forward "<\/textarea>")
                                  (re-search-backward "<\/text")))
              (buffer-substring-no-properties beginning ending)))))
    (insert bibtex-text)))

(defun bibtex-get-field-from-entry-under-cursor (field bibfile-buffer)
  "Get FIELD's value from bib entry in BIBFILE-BUFFER under cursor."
  (with-current-buffer bibfile-buffer
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (field-value (org-ref-reftex-get-bib-field field entry)))
        field-value))))

(defun klin-bibtex-get-field (field &optional key bibfile-path)
  "Return key value pair (FIELD, value) of a bib entry with KEY in BIBFILE-PATH.
Unless KEY is given, use the entry under cursor.
Unless BIBFILE-PATH is given, search in the current bib buffer."
  (unless key (setq key "negele98_quant"))
  (unless bibfile-path (setq bibfile-path (cdr (org-ref-get-bibtex-key-and-file key))))
  (unless bibfile-path (setq bibfile-path (expand-file-name "~/Dropbox/2TextBooks/.1-NegeleOrland-QuantumManyParticeSystems.pdf.bib")))

  (let* (entry)
    (with-temp-buffer
      (bibtex-mode)
      (insert-file-contents bibfile-path)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      ;; refresh the parsing of the keys
      (bibtex-parse-keys)
      ;; if just one, get it's key
      (unless key
        (if (= 1 (length (bibtex-global-key-alist)))
            (setq key (car (nth 0 (bibtex-global-key-alist)))))
        )
      (unless key (setq key "negele98_quanti")) ;; debugging

      ;; find the one you are looking for, after parsing keys
      (unless (bibtex-search-entry key nil 0)
        (message (concat "no key " key " found in " bibfile-path))
        nil
        (setq entry
              (bibtex-parse-entry)) ; sets cursor at the end of entry's last field
        (let ((field-value (org-ref-reftex-get-bib-field field entry)))
          (if (>= (length field-value) 1)
              field-value
            (message (concat "no field " field " found in " bibfile-path " -> " key))
            nil)) ;; sets cursor at the beginning of the entry's line
        ))))

(defun klin-open-pdf-from-bibtex (bibtexkey page)
  "Open BIBTEXKEY's pdf file on PAGE, respecting page offset."
  (let* ((file-page-offset (string-to-number (klin-bibtex-get-field "file-page-offset" bibtexkey)))
         (filepath (klin-bibtex-get-field "filepath" bibtexkey))
         (page (- (+ page file-page-offset) 1)))
    (open-pdf-document-new-frame filepath page)
    )
  )

(defun make-bibtex-file-for-pdf (&optional pdfpath)
  "For a pdf at PDFPATH, start making a bibtex entry."
  (interactive)
  ;; (unless pdfpath (setq pdfpath (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  ;; (unless isbn (setq isbn "0-7382-0052-2"))
  (let* ((basedir (file-name-directory pdfpath))
         (filename (file-name-nondirectory pdfpath))
         (bibtexfilename (concat "." filename ".bib")) ;; "hidden" file
         (bibfilepath (concat basedir bibtexfilename)))

    ;; check if pdf actually exists
    (unless (file-exists-p pdfpath)
      (unless (yes-or-no-p "PDF file doesn't exist, continue anyway?")
        (error "PDF file doesn't exist, chose to quit")))

    ;; this appends to a file (and creates one if there is none)
    (unless bibfilepath (setq bibfilepath (expand-file-name "~/Dropbox/stuff/1Book/testfile.txt")))
    (with-temp-buffer (write-region "" nil bibfilepath 'append))
    (klin-bibtex-edit-interactively bibfilepath nil)))

(defun on-bibtex-processing-frame-close-hf (&optional frame)
  "Called when FRAME is closed (if contained in list 'delete-frame-functions').
If the current frame of the bibtex batch processing is closed, you move on
to the next frame in the list."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (message (concat "calling on-bibtex-processing-frame-close-hf on frame "
                   (prin1-to-string frame)))
  (let* ( ;; (fitting-tuple (car (rassoc frame bib-processing-bibfiles-frames)))
         (fitting-tuple (car (delq nil
                                   (mapcar (lambda (cur-tuple)
                                             (if (eq (nth 1 cur-tuple) frame)
                                                 cur-tuple
                                               nil))
                                           bib-processing-bibfiles-frames)))))
    ;; if that frame is inside the bib-processing-bibfiles-frames,
    ;; delete the tuple from the list
    (if fitting-tuple
        (let* ((intended-bibfile-path (car fitting-tuple)))
          ;; close the intended-bibfile-buffer
          (let ((intended-bibfile-buffer (get-buffer (file-name-nondirectory intended-bibfile-path))))
            (if intended-bibfile-buffer
                (kill-buffer intended-bibfile-buffer)))
          ;; if the intended-bibfile is empty now (maybe after saving), delete it
          (if (string= ""
                       (with-temp-buffer
                         (insert-file-contents intended-bibfile-path)
                         (buffer-string)))
              (delete-file intended-bibfile-path))))
    ;; remove the currently processed tuple from the list
    (setq bib-processing-bibfiles-frames (delq fitting-tuple bib-processing-bibfiles-frames))
    ;; open up a new frame with klin-bibtex-edit-interactively,
    ;; processing the next (car) tuple
    (if (> (length bib-processing-bibfiles-frames) 0)
        (let* ((next-tuple (car bib-processing-bibfiles-frames))
               (bibfile-path (car next-tuple)))
          ;; open up the next one
          (klin-bibtex-edit-interactively bibfile-path))
      ;; if bib-processing-bibfile-frames is empty,
      ;; set the state variable to not processing any more
      (setq bib-processing-currently-processing
            nil)))
  ;; then, let it proceed to delete the frame
  )

(defun klin-bibtex-edit-interactively (&optional intended-bibfile-path alternative-bibtex-entry-str)
  "Visit INTENDED-BIBFILE-PATH, optionally carry along ALTERNATIVE-BIBTEX-ENTRY-STR.
Removes the file at INTENDED-BIBFILE-PATH if at the end, it remains empty."
  (interactive)
  ;; (unless intended-bibfile-path (setq intended-bibfile-path (expand-file-name "~/Dropbox/2TextBooks/.1-NegeleOrland-QuantumManyParticeSystems.pdf.bib")))

  ;; open the existing bibfile
  (let* ((tmpfilepath (make-temp-file "alternative-bibtex-entry")))
    (find-file-other-frame intended-bibfile-path)
    ;; ;; split it in two to get side-by-side view with another buffer
    ;; (progn
    ;;   (if (<= (length (window-list)) 1)
    ;;       (split-window-vertically))
    ;;   (other-window 1))
    (if alternative-bibtex-entry-str
        ;; if some text was given, open a temp file and insert it
        (progn
          (find-file tmpfilepath)
          (insert alternative-bibtex-entry-str))
      ;; else: propose the opportunity to manually run isbn-to-bibtex
      ;; to get a suggestion
      (message "you can run e.g. ask-for-isbn-suggest-and-insert to get a suggestion")
      ;; you could run e.g. ask-for-isbn-suggestion-and-insert ()
      )
    ;; for consecutive processing of individual bibtex files assoc. to single pdfs:
    ;; change the frame value assoc to the current intended-bibfile-path
    (let* ((replace-with-tuple `(,intended-bibfile-path
                                 ,(selected-frame)))
           (mylist bib-processing-bibfiles-frames))
      (let* ((i 0))
        (while (< i (length mylist))
          (if (string= intended-bibfile-path
                       (car (nth i mylist)))
              (progn
                (setf (nth i mylist) replace-with-tuple)
                (setq bib-processing-currently-processing
                      t)))
          (setq i (+ i 1)))))
    ;; add a function to delete-frame-functions that will handle the transition:
    ;; when kill-frame-and-buffers-within is called at some point, that will
    ;; kill the previous frame and open up the next one in the list
    (add-to-list 'delete-frame-functions 'on-bibtex-processing-frame-close-hf)))

(defun ask-for-isbn-suggestion-and-insert ()
  "Fill an empty bib file with isbn template."
  (interactive)
  (let* ((isbn (read-string "ISBN? (return to continue):")))
    (isbn-to-bibtex isbn (buffer-file-name (current-buffer)))))

(defun fix-file-page-offset (&optional key)
  "Asks for the KEY's entry's pdf file page offset, with the choice of opening the pdf."
  (interactive)
  (let* ((bib-buffer (current-buffer))
         ;; (bib-window (selected-window))
         ;; pdf-buffer
         pdf-frame
         pdf-filepath
         offset-number)
    (unless key
      (setq key (bibtex-get-field-from-entry-under-cursor
                 "=key=" bib-buffer)))
    ;; open up a pre-populated helm menu for pdf-filepath
    ;; in order to open up the pdf for checking
    (setq pdf-filepath
          (helm-read-file-name
           (concat "pdf filepath for " key ": ")
           ;; suggest (and search) the pdf path with helm
           :initial-input (let* ((filepath-field (bibtex-get-field-from-entry-under-cursor
                                                  "filepath" bib-buffer))
                                 (standard-folder-path (expand-file-name "~/Dropbox/2TextBooks/")))
                            (if (file-exists-p (expand-file-name filepath-field))
                                (expand-file-name filepath-field)
                              (if (file-exists-p standard-folder-path)
                                  standard-folder-path
                                "")))))
    (open-pdf-document-new-frame (expand-file-name pdf-filepath)
                                 1)
    ;; (setq pdf-buffer (current-buffer))
    (setq pdf-frame (selected-frame))
    (setq offset-number (call-interactively 'klin-ask-pdf-offset-number))
    (delete-frame pdf-frame)
    (pop-to-buffer bib-buffer)
    (bibtex-set-field "file-page-offset"
                      (number-to-string offset-number)))
  (bibtex-clean-entry)
  (bibtex-fill-entry))

(defun fix-filepath-field (&optional key)
  "Set filepath field of KEY's bib entry.
If KEY is not set, edit entry under cursor."
  (interactive)
  (let* (filepath)
    (unless key
      (setq key (bibtex-get-field-from-entry-under-cursor "=key="
                                                          (current-buffer))))
    (setq filepath (klin-get-pdf-filepath-for-bibtex-entry))
    (bibtex-set-field "filepath"
                      (s-replace (substitute-in-file-name "$HOME")
                                 "~"
                                 (substitute-in-file-name (expand-file-name filepath)))))
  (bibtex-clean-entry)
  (bibtex-fill-entry))


(defun bibtex-next-entry (&optional n)
  "Jump to the beginning of the next (N th next) bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
                     (bibtex-beginning-of-entry)))
    (forward-char)
    (bibtex-next-entry))
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

(defun bibtex-previous-entry (&optional n)
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

(defun check-pdfs-for-bib-file (&optional filepaths)
  "Mark some PDFs with helm (optional: give FILEPATHS)."
  (interactive)
  (unless filepaths
    (setq filepaths (helm-read-file-name "mark some pdfs: " :initial-input "~/Dropbox/"
                                         :marked-candidates t)))
  (when (> (length bib-processing-bibfiles-frames) 0)
    (message
     "Some bib files were still in the queue, waiting to be processed. We skip them, reinitializing the processing queue.")
    (setq bib-processing-bibfiles-frames nil))
  ;; assign the list of bibfiles to process to bib-processing-bibfiles-frames
  ;; fill frames with nil for the beginning
  (let* ((as (mapcar (lambda (arg)
                       (klin-pdf-filepath-to-bibtex-filepath arg))
                     filepaths))
         (prepared-bibfiles-frames-list (mapcar (lambda (ai)
                                                  (let ((temp-as-bs nil))
                                                    (setq temp-as-bs (append temp-as-bs
                                                                             `(,ai ,nil)))))
                                                as)))
    (setq bib-processing-bibfiles-frames (append bib-processing-bibfiles-frames prepared-bibfiles-frames-list)))

  ;; start with the first one, that initiates it; after closing that one,
  ;; there is a hook to open the 2nd one and so on.
  (let ((pdf-filepath (nth 0 filepaths)))
    (make-bibtex-file-for-pdf pdf-filepath)))

(provide 'klin-bibtex)
;;; klin-bibtex.el ends here
