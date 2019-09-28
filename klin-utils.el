;;; klin-utils.el --- utility functions for klin packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-tower>
;; Keywords: tools

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

;; This file provides utility functions for klin packages.

;;; Code:


(require 'bibtex)
;; (require 'parsebib)

(defun klin-try-to-fill-all-pdf-stuff-in-1-isbn ()
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
        (setq pdf-filepath (klin-get-pdf-filepath-for-bibtex-entry)))
    ;; now that we have the file path, open the pdf to find the isbn
    (message "We'll open up the pdf now. You could try to find the isbn.")
    (sleep-for 2)
    (find-file-other-frame pdf-filepath)))

(defun klin-try-to-fill-all-pdf-stuff-in-2-pdfstuff ()
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
      (fix-filepath-field))
    (unless (or (not (string= "" file-page-offset-field-str))
                (not file-page-offset-field-str))
      (fix-file-page-offset))
    )
  )

(defun klin-get-pdf-filepath-for-bibtex-entry (&optional key)
  "Try to find the pdf associated with the bib file.
KEY can be provided, but if not it will try to figure
the pdf filename out by the buffer file name."
  (interactive)

  ;; re-parse the bib buffer
  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
  (bibtex-parse-keys)

  (helm-read-file-name
   (concat "what is the corresponding pdf?"
           (if (or (not (string= "" key)) (not key))
               "(key: not set)"
             (concat "(key: " key " )"))
           ": ")
   :initial-input
   (let* ((filepath-field
           (bibtex-get-field-from-entry-under-cursor
            "filepath" (current-buffer)))
          (filename-guess-from-bibfile-name
           (klin-bibtex-filename-to-pdf-filename (buffer-name)))
          (standard-folder-path
           (expand-file-name "~/Dropbox/2TextBooks/"))
          (filepath-guess-from-bibfile-name-standard-folder
           (concat standard-folder-path
                   filename-guess-from-bibfile-name)))

     (if (and (not (string= filepath-field ""))
              (file-exists-p (expand-file-name filepath-field)))
         (expand-file-name filepath-field)
       (if (file-exists-p
            filepath-guess-from-bibfile-name-standard-folder)
           filepath-guess-from-bibfile-name-standard-folder
         (if (file-exists-p standard-folder-path)
             standard-folder-path
           "")))))

(defun klin-bibtex-filename-to-pdf-filename (bibtex-filename)
  "Convert BIBTEX-FILENAME to the standard associated pdf filename."
  ;; (interactive)
  (let* ( ;; (filename ".myname.pdf.bib") ;; test (checked using re-builder)
         (filename bibtex-filename))
    (string-match "^\.\\(+?.*\\)\.bib$" filename)
    (match-string 1 filename))))

(defun klin-pdf-filename-to-bibtex-filename (pdf-filename)
  "Convert PDF-FILENAME to the standard associated bib filename."
  (concat "." pdf-filename ".bib"))

(defun klin-pdf-filepath-to-bibtex-filepath (pdf-filepath)
  "Convert PDF-FILEPATH (not pdf-filename) to a bib file's path."
  (interactive)
  (let* ((pdf-filename (file-name-nondirectory pdf-filepath))
         (bibtex-filename (klin-pdf-filename-to-bibtex-filename pdf-filename))
         (bibtex-filepath (concat (file-name-directory pdf-filepath)
                                  bibtex-filename)))
    bibtex-filepath))

(defun klin-ask-pdf-offset-number (num)
  "Prompt user to enter a number NUM, with input history support."
  (interactive
   (list
    (read-number "pdf page offset number: ")))
  (message "number is %s." (number-to-string num))
  num)


(provide 'klin-utils)
;;; klin-utils.el ends here
