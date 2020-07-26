;;; klin-pdf-toggle.el --- Toggle a pdf window to be the only window and toggle back  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
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

;; ------- within a normal pdf
(define-key pdf-view-mode-map (kbd "C-M-, l") 'klin-pdf-pdfview-store-link)

(defun klin-delete-other-windows-show-pdf-comfortably ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (set-window-parameter (selected-window) 'pdf-toggle-param
                          (list (current-window-configuration) (buffer-file-name)))
    (delete-other-windows)
    (pdf-view-redisplay)
    (pdf-view-set-comfortable-reading-size)
    (pdf-view-redisplay)))

(defun klin-delete-other-windows-show-pdf-comfortably-winner-undo ()
  (interactive)
  (let* ((pdf-window (selected-window))
         (toggle-param (window-parameter (selected-window)
                                         'pdf-toggle-param)))
    (set-window-configuration (car toggle-param))
    (set-window-parameter (selected-window)
                          'pdf-toggle-param
                          (list nil
                                (buffer-file-name)))

    (when (eq (selected-window) pdf-window)
      (pdf-view-redisplay)
      (pdf-view-set-comfortable-reading-size)
      (pdf-view-redisplay))))

(defun klin-toggle-pdf-only-view ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (let* ((pdf-window (selected-window))
           (toggle-param (window-parameter (selected-window)
                                           'pdf-toggle-param)))
      (if (and (not (car toggle-param))
               (not (nth 1 toggle-param)))
          (klin-delete-other-windows-show-pdf-comfortably)
          (if (and (car toggle-param)
                   (string-equal (nth 1 toggle-param)
                                 (buffer-file-name)))
              (klin-delete-other-windows-show-pdf-comfortably-winner-undo)
            (klin-delete-other-windows-show-pdf-comfortably)))
      )))

;; (defun klin-toggle--only-view ()
;;   (interactive)
;;   (when (eq major-mode 'pdf-view-mode)
;;     (let* ((pdf-window (selected-window))
;;            (toggle-param (window-parameter (selected-window)
;;                                            'pdf-toggle-param)))
;;       (if toggle-param
;;           (klin-delete-other-windows-show-pdf-comfortably-winner-undo)
;;         (klin-delete-other-windows-show-pdf-comfortably)))))


(provide 'klin-pdf-toggle)
;;; klin-pdf-toggle.el ends here
