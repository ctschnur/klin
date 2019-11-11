;;; klin-xournalpp.el --- This script procedurally creates an xournalpp file from a pdf background.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-thinkpad>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dash)

(defun cs-create-xournalpp-file-for-pdf (input-pdf-filepath output-xopp-filepath)
  "The command is
pdfinfo source-pdf.pdf -l -1 | grep Page | grep size | awk '{ print $2; print $4; print $6}'"
  (let* ((pdfinfo-str (shell-command-to-string (concat "pdfinfo " (prin1-to-string input-pdf-filepath) " -l -1 | grep Page | grep size | awk '{ print $2; print $4; print $6}'"))) ; shows dimensions of all pages

         (n-w-h-triples (-partition 3
                                    (mapcar (lambda (elem)
                                              (string-to-number elem))
                                            (split-string pdfinfo-str "\n"))))
         (ctr 0))
    (with-temp-buffer
      (delete-region (point-min)
                     (point-max))
      (insert "<?xml version=\"1.0\" standalone=\"no\"?>
<xournal creator=\"Xournal++ 1.0.15\" fileversion=\"4\">
<title>Xournal++ document - see https://github.com/xournalpp/xournalpp</title>")
      (insert "<preview></preview>
")
      (cl-loop for
               nwh
               in
               n-w-h-triples
               collect
               (insert (concat "<page width=\""
                               (number-to-string (nth 1 nwh))
                               "\" height=\""
                               (number-to-string (nth 2 nwh))
                               "\">
      <background type=\"pdf\" "
                               (when (eq (nth 0 nwh) 1)
                                 (concat "domain=\"absolute\" filename=\""
                                         input-pdf-filepath "\""))
                               " pageno=\""
                               (number-to-string (nth 0 nwh))
                               "ll\"/>
      <layer/>
      </page>
")))
      (insert "</xournal>")
      (write-region (point-min) (point-max)
                    output-xopp-filepath))))



;; https://github.com/toroidal-code/dom.el
;; (require 'dom)

;; (defun cb/copy-template-files-to-target-dir (&optional source-xopp-file-path target-xopp-file-path)
;;   (unless source-xopp-file-path
;;     (setq source-xopp-file-path "~/Desktop/plg/source.xopp"))
;;   (unless target-xopp-file-path
;;     (setq target-xopp-file-path "~/Desktop/plg/target-folder/target.xopp"))

;;   (let* ((target-gz-file-path (concat (file-name-sans-extension target-xopp-file-path)
;;                                       ".gz")))
;;     (copy-file source-xopp-file-path target-gz-file-path 'override)
;;     ;; ACTUALLY: unzip not necessary. Just make a copy to .gz, then read it as utf-8 (emacs selects that
;;     ;; automatically for this file ending and the supplied data)
;;     ;; later, renameit back to .xopp
;;     (let* ((gz-filename (concat (file-name-base target-xopp-file-path)
;;                                 ".gz"))
;;            (gz-filepath (concat (file-name -directory target-xopp-file-path)
;;                                        gz-filename)))
;;       (cs-replace gz-filepath gz-filepath
;;                   "background" "filename" "other-abs-file-path"))
;;     ;; now load the file, and do operations on it
;;     (with-temp-buffer
;;       (insert-file-contents tmp-text-xopp-file)
;;       ;; search forward to regex
;;       )))

;; (defun cs-replace (source-xml-filepath target-xml-filepath tag attribute replacement-field-string)
;;   "Replace an attribute in a tag (only the first occurence, both times)."
;;   (save-excursion
;;     (with-temp-buffer
;;       (insert-file-contents source-xml-filepath)
;;       (goto-char (point-min))
;;       (let* ((tag-to-search-in tag)
;;              (attribute-to-replace attribute)
;;              (text-to-replace-field-with replacement-field-string)
;;              (field-raw-string-start (save-excursion
;;                                        (re-search-forward (concat tag-to-search-in
;;                                                                   ".+" attribute-to-replace
;;                                                                   "=\"\\([^\"]*\\)\""))
;;                                        (match-beginning 1)))
;;              (field-raw-string-end (match-end 1)))

;;         (buffer-substring-no-properties field-raw-string-start
;;                                         field-raw-string-end)
;;         (delete-region field-raw-string-start
;;                        field-raw-string-end)
;;         (save-excursion
;;           (goto-char field-raw-string-start)
;;           (insert text-to-replace-field-with)))
;;       (write-region (point-min) (point-max)
;;                     target-xml-filepath))))




(provide 'klin-xournalpp)
;;; klin-xournalpp.el ends here
