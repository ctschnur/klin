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

(require 'parsebib)
(require 's)

(defun klin-utils-pdf-get-ancillary-dir-path (pdf-filepath)
  "Get the ancillary directory for a pdf."
  ;; setting pdf-filepath
  ;; (if pdf-filepath
  ;;     (progn
  ;;       (unless (file-exists-p pdf-filepath)
  ;;         (error "The pdf file doesn't exist."))
  ;;       (if (file-exists-p (concat pdf-filepath)))
  ;;       (setq pdf-filepath (expand-file-name pdf-filepath))
  ;;       )
  ;;   (if (string-equal (file-name-extension (buffer-file-name))
  ;;                     "pdf")
  ;;       (setq pdf-filepath (buffer-file-name))
  ;;     (error "Unable to determine pdf-filepath")))
  (concat (file-name-directory pdf-filepath)
          (file-name-as-directory (file-name-base pdf-filepath)))
  )

(defun klin-utils-pdf-get-notes-dir-path (pdf-filepath)
  "From within a pdf buffer, or given a PDF-FILEPATH, get the relative notes file path.
If ABSOLUTE is non-nil, get the absolute path.
The directory structure looks like this:

this.pdf
this/ (ancillary dir)
     |-> notes/
           |-> notes.org
               notes.bib (automatically created when trying to cite sth in notes.org)
               graphics/
         self.bib"
  ;; ;; setting pdf-filepath
  ;; (if pdf-filepath
  ;;     (setq pdf-filepath (expand-file-name pdf-filepath))
  ;;   (if (string-equal (file-name-extension (buffer-file-name))
  ;;                     "pdf")
  ;;       (setq pdf-filepath (buffer-file-name))
  ;;     (error "Unable to determine pdf-filepath")))

  (file-name-as-directory
   (concat
    (klin-utils-pdf-get-ancillary-dir-path pdf-filepath)
    "notes"))
  )

(defun klin-utils-pdf-get-self-bib-file-path (pdf-filepath)
  (concat
   (klin-utils-pdf-get-ancillary-dir-path pdf-filepath)
   "self.bib")
  )

(defun klin-utils-pdf-get-org-notes-file-path (pdf-filepath)
  (concat
   (klin-utils-pdf-get-notes-dir-path pdf-filepath)
   "notes.org")
  )

;; (defun klin-utils-pdf-get-org-notes-bibliography-file-path (org-notes-file-path)
;;   (concat
;;    (file-name-directory org-notes-file-path)
;;    "references.bib"))

(defun klin-utils-ask-to-create-dir (directory)
  "Only if it's not already a directory."
  (setq directory (file-name-as-directory directory))
  (unless (file-exists-p directory)
    (setq directory (helm-read-file-name "Create directory: "
                                         :initial-input directory))
    (make-directory directory 'parents)))

(defun klin-utils-isbn-to-bibtex-ottobib-insert (isbn)
  "Insert ottobib bib entry suggestion for ISBN."
  (interactive "sISBN: ")
  ;; (unless isbn (setq isbn "0195117972"))
  (let* ((tmpfilename (make-temp-file "ottobib"))
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

(defun klin-utils-get-reduced-file-path (full-filepath)
  "Get the reduced pdf filepath from FULL-FILEPATH.
The reduced pdf filepath is of the form ~/... .
The full-filepath is of the form /home/[USER]/... ."
  (s-replace (substitute-in-file-name "$HOME")
             "~"
             (substitute-in-file-name (expand-file-name full-filepath))))

(defun kill-frame-and-buffers-within ()
  "Delete all buffers shown in the selected frame.
In general, buffers aren't strictly associated to
specific frames.  Also, kill the selected frame."
  (interactive)
  ;; get the buffers within the currently selected frame
  (let* ((buffers-within-frame (cl-delete-duplicates (mapcar #'window-buffer (window-list))))
         ;; (frame (selected-frame))
         (remaining (delq nil (mapcar (lambda (buf)
                                        (let ((this-window-buffer (get-buffer-window buf)))
                                          (if (not (kill-buffer buf))
                                              buf
                                            (delete-window this-window-buffer)
                                            nil)))
                                      buffers-within-frame)))
         ;; (intersection (cl-intersection buffers-within-frame remaining))
         ;; (killed-buffers (cl-set-exclusive-or buffers-within-frame remaining))
         )
    (if (= (length remaining) 0)
        (delete-frame))))

(provide 'klin-utils)
;;; klin-utils.el ends here
