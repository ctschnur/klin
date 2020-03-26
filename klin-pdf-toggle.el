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
                          (current-window-configuration))
    (delete-other-windows)
    (pdf-view-redisplay)
    (pdf-view-set-comfortable-reading-size)
    (pdf-view-redisplay)))

(defun klin-delete-other-windows-show-pdf-comfortably-winner-undo ()
  (interactive)
  (let* ((pdf-window (selected-window))
         (toggle-param (window-parameter (selected-window) 'pdf-toggle-param)))
    (when toggle-param
      (set-window-configuration toggle-param)
      (set-window-parameter (selected-window)
                            'pdf-toggle-param
                            nil))

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
      (if toggle-param
          (klin-delete-other-windows-show-pdf-comfortably-winner-undo)
        (klin-delete-other-windows-show-pdf-comfortably))
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

(defun my-add-pdf-view-comfortable-read-key ()
  (interactive)
  (evil-define-key 'normal pdf-view-mode-map (kbd "B") 'pdf-history-backward)
  (evil-define-key 'normal pdf-view-mode-map (kbd "F") 'pdf-history-forward)
  (evil-define-key 'normal pdf-view-mode-map (kbd "R") 'klin-toggle-pdf-only-view)
  (evil-define-key 'normal pdf-view-mode-map (kbd "S") 'klin-clone-into-split-window)
  (evil-define-key 'normal pdf-view-mode-map (kbd "r") 'pdf-view-set-comfortable-reading-size)
  (evil-define-key 'normal pdf-view-mode-map (kbd "E") 'cs-open-org-notes)
  (add-hook 'pdf-view-mode-hook #'evil-normalize-keymaps)
  ;; (define-key pdf-view-mode-map (kbd "r") 'pdf-view-set-comfortable-reading-size)
  )

(add-hook 'pdf-view-mode-hook #'my-add-pdf-view-comfortable-read-key)
;; (add-hook 'pdf-view-mode-hook #'pdf-view-set-comfortable-reading-size t)



(provide 'klin-pdf-toggle)
;;; klin-pdf-toggle.el ends here
