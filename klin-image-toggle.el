;;; klin-image-toggle.el --- toggle image-mode window to be the only window and toggle back  -*- lexical-binding: t; -*-

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

;; ------- within a normal image

(defun image-mode-set-comfortable-reading-size ()
  (interactive)
  (image-transform-reset))

(defun klin-delete-other-windows-show-image-comfortably ()
  (interactive)
  (when (eq major-mode 'image-mode)
    (set-window-parameter (selected-window) 'image-toggle-param
                          (current-window-configuration))
    (delete-other-windows)
    ;; (pdf-view-redisplay)
    (image-mode-set-comfortable-reading-size)
    ;; (pdf-view-redisplay)
    ))

(defun klin-delete-other-windows-show-image-comfortably-winner-undo ()
  (interactive)
  (let* ((image-window (selected-window))
         (toggle-param (window-parameter (selected-window) 'image-toggle-param)))
    (when toggle-param
      (set-window-configuration toggle-param)
      (set-window-parameter (selected-window)
                            'image-toggle-param
                            nil))

    (when (eq (selected-window) image-window)
      ;; (pdf-view-redisplay)
      (image-mode-set-comfortable-reading-size)
      ;; (pdf-view-redisplay)
      )))

(defun klin-toggle-image-only-view ()
  (interactive)
  (when (eq major-mode 'image-mode)
    (let* ((image-window (selected-window))
           (toggle-param (window-parameter (selected-window)
                                           'image-toggle-param)))
      (if toggle-param
          (klin-delete-other-windows-show-image-comfortably-winner-undo)
        (klin-delete-other-windows-show-image-comfortably)))))

;; (defun klin-toggle--only-view ()
;;   (interactive)
;;   (when (eq major-mode 'pdf-view-mode)
;;     (let* ((pdf-window (selected-window))
;;            (toggle-param (window-parameter (selected-window)
;;                                            'pdf-toggle-param)))
;;       (if toggle-param
;;           (klin-delete-other-windows-show-pdf-comfortably-winner-undo)
;;         (klin-delete-other-windows-show-pdf-comfortably)))))

(defun my-add-image-comfortable-read-key ()
  (interactive)
  ;; (evil-define-key 'normal image-mode-map (kbd "B") 'pdf-history-backward)
  ;; (evil-define-key 'normal image-mode-map (kbd "F") 'pdf-history-forward)
  (evil-define-key 'normal image-mode-map (kbd "R") 'klin-toggle-image-only-view)
  ;; (evil-define-key 'normal image-mode-map (kbd "S") 'klin-clone-into-split-window)
  (evil-define-key 'normal image-mode-map (kbd "r") 'image-transform-reset)
  (add-hook 'image-mode-hook #'evil-normalize-keymaps))

(add-hook 'image-mode-hook #'evil-mode)
(add-hook 'image-mode-hook #'my-add-image-comfortable-read-key)

(provide 'klin-image-toggle)
;;; klin-image-toggle.el ends here
