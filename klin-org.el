;;; klin-org.el --- Klin functions to call from org document  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-tower>
;; Keywords:

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

;; These functions should include:
;; - function to highlight and open the next dynamic link to a pdf page
;;
;; - function to insert the latest bookmark created from a pdf page as a dynamic link
;; (optional:
;;  - function to create custom org link as a dynamic link)

;;; Code:

(require 'pdf-view)
(require 'org)
(require 'klin-bibtex)

(defun klin-org-open-link (mylist)
  "Open link with link info stored in MYLIST."
  (let* (; (linktyp (nth 0 mylist))
         (bibtexkey (nth 1 mylist))
         (description (nth 2 mylist))
         page-str
         (collective-bib-file (car (klin-org-org-ref-find-bibliography-fullfilenames))))
    (string-match "p\\.\\s-*\\([-0-9]*\\)" description)
    (setq page-str (match-string 1 description))

    ;; now go into the bibtex buffer and execute from there
    (with-bib-file-buffer collective-bib-file
                          (klin-bibtex-open-pdf-from-bibtex bibtexkey (string-to-number page-str)))))


(defun klin-org-open-bibliographys-pdfs ()
  "Inside an org buffer, search it's referenced bib file.
And therein, search for pdfs that can be opened.  Then query which ones to open."
  (interactive)
  (let* ((bib-file (expand-file-name (substring-no-properties (nth 0 (org-ref-find-bibliography))))))
    (with-temp-buffer
      (insert-file-contents bib-file)
      (bibtex-mode)
      (klin-bibtex-reparse-bib-buffer)
      (klin-bibtex-open-pdfs-in-bibtex-file)))
  )


(defun klin-org-get-link-text-at-point ()
  "Get the text (description) of the org link at point."
  (let* ((type (org-element-context)))
    (if (eq (car (org-element-context)) 'link)
        (buffer-substring-no-properties (org-element-property :begin type)
                                        (org-element-property :end type)))))


(require 'dash)

(defun cs/get-value-with-lowest-abs-value (list)
  "Get value with lowset absolute value in LIST."
  (let* ((mylist list)
         (cur-min-abs-val (car mylist))
         (ctr 0))
    (while (< ctr (length mylist))
      (if (< (abs (nth ctr mylist)) cur-min-abs-val)
          (setq cur-min-abs-val (nth ctr mylist)))
      (setq ctr (+ ctr 1)))
    cur-min-abs-val))

(defun klin-org-get-link-data-nearest-to-point ()
  "Get the link info list nearets to point."
  (let* (; check if at point there is a link
         (re "\\[\\[\\(.*?\\):\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
         (nearestlink-string (klin-org-get-link-text-at-point))
         poscur
         nextlink-match-beginning
         ;; nextlink-match-end
         nextlink-match-string
         ;; prevlink-match-beginning
         prevlink-match-end
         prevlink-match-string
         ;; nearestlink-beginning
         ;; nearestlink-string
         )
    (if (not nearestlink-string)
        (progn
          (setq poscur (point))
          (save-excursion
            (re-search-forward re nil t 1)
            (setq nextlink-match-beginning (match-beginning 0))
            ;; (setq nextlink-match-end (match-end 0))
            (setq nextlink-match-string (match-string-no-properties 0)))
          (save-excursion
            (re-search-backward re nil t 1)
            ;; (setq prevlink-match-beginning (match-beginning 0))
            (setq prevlink-match-end (match-end 0))
            (setq prevlink-match-string (match-string-no-properties 0)))
          ;; find the nearest one
          (if (< (abs (- prevlink-match-end poscur)) (abs (- nextlink-match-beginning poscur)))
              (progn
                ;; (setq nearestlink-beginning prevlink-match-beginning)
                (setq nearestlink-string prevlink-match-string))
            (progn
              ;; (setq nearestlink-beginning nextlink-match-beginning)
              (setq nearestlink-string nextlink-match-string)))))

    ;; apply the regex again to the nearest string and copy the data
    (string-match re nearestlink-string)
    (list (match-string 1 nearestlink-string)
          (match-string 2 nearestlink-string)
          (match-string 3 nearestlink-string))))

;; (defun klin-org-get-link-data-nearest-to-point ()
;;   "Get the link info list nearets to point."
;;   (let* (; check if at point there is a link
;;          (link-regexs (list
;;                        '(re-klin
;;                         "\\[\\[\\(.*?\\):\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
;;                        ;; '(re-no-type
;;                        ;;  "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
;;                        ))
;;          (nearestlink-string (klin-org-get-link-text-at-point))
;;          (cur-pos (point))
;;          nextlink-match-beginning-pos
;;          nextlink-match-string
;;          prevlink-match-end-pos
;;          prevlink-match-string
;;          delta)

;;     ;; if it's not at point, look for the closest, then set point to that
;;     (unless nearestlink-string
;;       ;; do the re-search-forward and get a list of delta-pos
;;       (setq delta (cs/get-value-with-lowest-abs-value
;;                    (mapcar
;;                     (lambda (regex-type-regex-tuple)
;;                       (remove nil
;;                               (let* ((regex-type (nth 0 regex-type-regex-tuple))
;;                                      (regex (nth 1 regex-type-regex-tuple)))
;;                                 (cs/get-value-with-lowest-abs-value
;;                                  (remove nil
;;                                          (list (save-excursion
;;                                                  (re-search-forward regex nil t 1)
;;                                                  (setq nextlink-match-beginning-pos (match-beginning 0))
;;                                                  (- nextlink-match-beginning-pos cur-pos))
;;                                                (save-excursion
;;                                                  (re-search-backward regex nil t 1)
;;                                                  (setq prevlink-match-end-pos (match-end 0))
;;                                                  (- prevlink-match-end-pos cur-pos))))))))
;;                     link-regexs)))
;;       ;; go to that delta and match the string
;;       )

;;     ;; apply the regex again to the nearest string and copy the data
;;     (string-match re-klin nearestlink-string)
;;     (list (match-string 1 nearestlink-string)
;;           (match-string 2 nearestlink-string)
;;           (match-string 3 nearestlink-string))))


(defun klin-org-org-ref-find-bibliography-fullfilenames (&optional org-buffer)
  "Find the bibliography associated to an org file opened in ORG-BUFFER.
If ORG-BUFFER isn't given, the current buffer is assumed."
  (interactive)
  (unless org-buffer
    (progn
      ;; if the current buffer from wich this function is called is an org buffer, then use that one
      (if (or (eq major-mode 'org-mode)
              (string= "org"
                       (file-name-extension (buffer-name))))
          (setq org-buffer (current-buffer))
        (message "not called from an org file and option org-buffer not provided. "))))
  ;; (unless org-buffer (setq org-buffer (get-buffer "main.org"))) ; debugging

  ;; find partial filenames relative to org buffer in which e.g. addbibresources are defined
  (with-current-buffer org-buffer
    (let ((i 0)
          (list-of-property-strings (org-ref-find-bibliography))
          list-of-full-referenced-bibtex-filepaths)
      (while (< i (length list-of-property-strings))
        (let* ((bibresource-file-name (substring-no-properties (nth i list-of-property-strings)))
               (bibresource-base-dir (file-name-directory (buffer-file-name)))
               (full-referenced-bibtex-filepath (concat bibresource-base-dir bibresource-file-name)))
          (setq list-of-full-referenced-bibtex-filepaths
                (append list-of-full-referenced-bibtex-filepaths
                        `(,full-referenced-bibtex-filepath))))
        (setq i (+ i 1)))
      list-of-full-referenced-bibtex-filepaths)))


;; --------- org-mode link integration

;; TODO: write my own proper klin link type that integrates
;; with org-store-link and org-insert-link instead of custom functions
;; and make that link type work with org-ref, too
(defun override-org-pdfview-store ()
  "TODO: Insert a link created the pdfview mode."
  (interactive)
  ;; package org-pdfview with org-pdfview.el
  ;; override a function there
  (eval-after-load "org-pdfview"
    (defun org-pdfview-store-link ()
      (when (eq major-mode 'pdf-view-mode)
        (let* ((type "cite")
               ;; (filename (file-name-nondirectory (buffer-file-name)))
               ;; make sure there is a proper bibtex file associated with it, otherwise create it
               (bibtexkey klin-bibtex)
               (link (concat type ":" bibtexkey))
               ;; - first of all, don't override that function, but make your own
               ;;   custom function with a global list of stored link property lists
               ;; - Map the store-link function to it's own global key
               ;; - on klin-store-link, look if the pdf has a bibtex file sitting in it's folder.
               ;;   store the path to that bibtex file alongside the link properties in
               ;;   the datastructure.
               ;; - on klin-insert-link, open up a helm-source with the candidates from the globally
               ;;   stored list
               ;; - then, at insertion, search in the org document's collective bibtex file,
               ;;   for a corresponding entry with the same pdf filepath as the original pdf
               ;; - at insertion of a new entry, check if the key is already
               ;;   used by another entry. If yes,
               ;;   open the bibfile buffer and search and highlight both entries to manually make
               ;;   a change.
               ;;   (can happen for good reason, e.g. if you have the same author publishing
               ;;   two cited things in the same year).
               ;; search a pdf's bibtex entry in the org document's associated bibtex
               ;; file (based on filepath).
               ;; If that filepath is found, take it's bibtexkey.
               ;; If that filepath is not found, ask for a bibtex file to go check in,
               ;; with 1st choice based on the pdf's accompanying bib file's path.
               ;; if none at all is found, ask to create one associated to that pdf.
               (page-offset 0)
               (page-in-pdf (pdf-view-current-page))
               (page-in-print (- page-in-pdf page-offset))
               (description (concat "p."
                                    (number-to-string page-in-print))))
          (org-store-link-props :type type
                                :link link
                                :description description))))))

;; -------- user interaction for citation insertion and opening reference

(defun klin-org-open-link-nearest-to-point ()
  "If cursor is somewhere in the text, search the nearest link and open it.
By default, don't open a new frame, and maximize the window."
  (interactive)
  (unless (klin-org-link-jump 'no-error) ; first try the conventional org link
    (klin-org-open-link (klin-org-get-link-data-nearest-to-point))))

(defun klin-org-create-and-add-bibliography (&optional bib-path)
  "In an org mode file, create and add a bibliography at BIB-PATH."
  (interactive)
  (unless bib-path
    (setq bib-path (helm-read-file-name "Create and/or reference a bib file: "
                                        :initial-input (concat (file-name-directory (buffer-file-name))
                                                               (file-name-base (buffer-file-name))
                                                               ".bib"))))

  ;; touch the file in a possibly not already created directory
  (with-temp-buffer
    (klin-utils-ask-to-create-dir (file-name-directory bib-path))
    (find-file bib-path)
    (save-buffer)
    (kill-buffer))

  (if (file-exists-p bib-path)
      (progn
        ;; insert it at the top as a latex reference
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          ;; use LATEX_HEADER_EXTRA to specify that this instruction
          ;; is to be ignored by latex fragement preview
          ;; else, there is a problem with the preview: if you
          ;; add sth. else than \usepackage{example}, e.g.
          ;; \sthelse{example} it renders
          ;; "example" in front of every latex fragment
          (insert "#+LATEX_HEADER_EXTRA: \\addbibresource{"
                  (file-name-base bib-path)
                  ".bib}\n")))
    (error "File wasn't created")))

(defun klin-org-insert-latex-overhead ()
  "Inserts the latex overhead into an org-mode buffer.
So that it can be compiled into a latex file with references."
  (interactive)
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    ;; use LATEX_HEADER_EXTRA to specify that this instruction
    ;; is to be ignored by latex fragement preview
    ;; else, there is a problem with the preview: if you
    ;; add sth. else than \usepackage{example}, e.g.
    ;; \sthelse{example} it renders
    ;; "example" in front of every latex fragment
    (insert
     "#+LATEX_HEADER: \\usepackage[backend=biber, style=alphabetic, sorting=nyt]{biblatex}
#+LATEX_HEADER: \\usepackage[ngerman]{babel}
#+LATEX_HEADER: \\usepackage[margin=1.15in]{geometry}
#+LATEX_HEADER: \\usepackage{physics}
#+LATEX_HEADER: \\usepackage{booktabs}
#+OPTIONS: toc:nil
#+BEGIN_EXPORT latex
\\today
#+END_EXPORT
")))

(defun klin-org-insert-pdf-link ()
  "Insert a formatted link to a pdf."
  (interactive)
  ;; search the org documents bibtex file for an entry with
  ;; matching filepath
  (unless (bound-and-true-p klin-pdfview-stored-link)
    (message "No stored link available for insertion.")
    nil)

  (let* ((collective-bibtex-filepath
          (let* ((bib-path
                  (car (klin-org-org-ref-find-bibliography-fullfilenames))))
            (if (or (not bib-path) (not (file-exists-p bib-path)))
                (progn
                  ;; ask to create and add a bibliography
                  (klin-org-create-and-add-bibliography bib-path))
              )
            bib-path))
         (reduced-pdf-filepath
          (klin-utils-get-reduced-file-path (klin-pdf-link-data-pdf-filepath
                                     klin-pdfview-stored-link)))
         (pdf-page (klin-pdf-link-data-pdf-page
                    klin-pdfview-stored-link))
         key
         offset
         print-page
         ;; link-text
         pdf-filepath-in-collective-bibfile-p)

    ;; parse the collective bib file for this filename
    ;; and get other stuff like key, and page offset
    (with-temp-buffer
      (insert-file-contents collective-bibtex-filepath)
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-parse-keys)
      (goto-char 0)

      (if (not (re-search-forward reduced-pdf-filepath nil t))
          (klin-bibtex-set-bibfiles-for-pdfs `(,(expand-file-name reduced-pdf-filepath)))
        (setq pdf-filepath-in-collective-bibfile-p t)
        (setq key (bibtex-get-field-from-entry-under-cursor "=key="))
        (setq offset (string-to-number
                      (bibtex-get-field-from-entry-under-cursor
                       "file-page-offset")))
        (setq print-page (- pdf-page offset)))
      )

    (if pdf-filepath-in-collective-bibfile-p
        (insert (concat "["
                        "[cite:" key "]"
                        "[p." (number-to-string
                               print-page) "]"
                               "]")))))


;; --------- jumping into bib file

(defun klin-org-action-jump-to-collective (candidate)
  "Jump to file CANDIDATE."
  ;; debugging
  ;; (message "hello")
  ;; (print candidate)
  ;; (print (car candidate))
  (find-file (car candidate))
  )

(defvar helm-source-klin-bibtex-jump-to-collective
      '((name . "jump to collective bibtex file:")
        (candidates . klin-bibtex-get-collective-bibtex-files)
        (action . (lambda (candidate)
                    (klin-org-action-jump-to-collective candidate)))))

(defun klin-bibtex-jump-to-collective-bib-file ()
  "Jump to collective bibtex file."
  (interactive)
  (helm :sources '(helm-source-klin-bibtex-jump-to-collective)))

;; --------

;; --------

;; klin open org-mode link as you would a klin link
;; (org-element-at-point)
;;

(require 'klin-optional)

(defun klin-org-link-jump (dont-issue-error)
  "Open Dired for directory of file link at point."
  (interactive)
  (let* ((el (org-element-lineage (org-element-context) '(link) t))
         (filepath (org-element-property :path el)))
    (if (not (and el (equal (org-element-property :type el) "file")))
        (if dont-issue-error
            nil
          (user-error "Not on file link"))

      (let* ((my-open-the-annotated-version-first t))
        (if my-open-the-annotated-version-first
            ;; check if there is an annotated version
            (let* ((annotated-pdf-filepath (concat (file-name-directory filepath)
                                                   (my-get-freehand-note-annotating-filename-base (file-name-base filepath))
                                                   ".pdf")))
              (if (file-exists-p annotated-pdf-filepath)
                  (progn
                    (setq filepath annotated-pdf-filepath)
                    (message "Showing the *annotated* version"))))))

      (open-pdf-document-other-frame-or-window filepath nil nil -1)
      t)))

;; --------

(provide 'klin-org)
;;; klin-org.el ends here
