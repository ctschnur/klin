;;; klin-optional.el --- klin-optional.el                      -*- lexical-binding: t; -*-

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

;; Here, I put some functions that can be useful to the workflow
;; but are not cure functionality of klin
;; (e.g. interfacing with org-noter, according to my personal convention)

;;; Code:

;; if a pdf in my library is called example.pdf, then
;; it should have an associated notes file .example.pdf.org


(require 'org-noter)

(defun klin-optional--add-assoc-notes-file-name-sugg (arg)
  "Add the suggestion .example.pdf.org to org-noter-default-notes-file-names.
If org-noter is started from within a pdf view (my workflow usually),
help it find my standard associated pdf notes org file (my convention),
within the same directory."
  (if (eq major-mode 'pdf-view-mode)
      (setq org-noter-default-notes-file-names
            (append org-noter-default-notes-file-names
                    (list (concat "." (file-name-base (buffer-file-name)) ".org"))))))

(add-function :before (symbol-function 'org-noter) #'klin-optional--add-assoc-notes-file-name-sugg)



;; ------- toying around with buttons

(defun wh/help-hello-world ()
  (interactive)
  (with-help-window (help-buffer)
    (princ "foo_bar is a function.\n\nIt does stuff.")
    (shrink-window 1)))

(defun button-pressed (button)
  (message (format "Button pressed!")))

(define-button-type 'custom-button
  'action 'button-pressed
  'follow-link t
  'help-echo "Click Button"
  'help-args "test"

  (make-button 1 10 :type 'custom-button))

;; --------

(provide 'klin-optional)
;;; klin-optional.el ends here
