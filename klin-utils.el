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
(require 'parsebib)
;; it shouldn't use (require 'klin-bibtex)

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




(provide 'klin-utils)
;;; klin-utils.el ends here
