;;; klin-pdf-frames.el --- PDF display and frame management functions for klin libraries  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <>
;; Keywords: frames, tools

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

;; - function to store a link to the current pdf filepath and page
;;   in a global variable

;;; Code:

(require 'frame-cmds)

(require 'helm)
(require 'pdf-view)

(cl-defstruct klin-pdf-link-data pdf-filepath pdf-page)

(defvar klin-pdfview-stored-link nil
  "Store a link to a pdf filepath and a page.")

(defun klin-pdf-pdfview-store-link ()
  "Store custom link data of currently shown pdf in klin-pdfview-stored-link.
Run this function from a pdfview buffer."
  (interactive)
  (setq klin-pdfview-stored-link
        (make-klin-pdf-link-data
         :pdf-filepath (expand-file-name (buffer-file-name))
         :pdf-page (pdf-view-current-page)))
  (print klin-pdfview-stored-link))

(defun klin-get-all-pdf-buffers ()
  "Get list of all pdf buffers."
  (interactive)
  (let ((i 0)
        (pdfbuffers (make-list 0 0))
        bufname)
    (while (< i (length (buffer-list)))
      (setq bufname (buffer-name (nth i (buffer-list))))
      (if (string-match-p ".pdf$" bufname)
          (setq pdfbuffers (append pdfbuffers `(,bufname))))
      (setq i (+ i 1)))
    pdfbuffers))

(defvar klin-helm-source-make-pdfs-visible
  "A helm source to get all pdf buffers and make them visible."
      '((name . "make visible and bring to front PDF buffer(s)")
        (candidates . klin-get-all-pdf-buffers)
        (action . (lambda (candidate)
                    (klin-pdf-make-pdf-frame-visible candidate)
                    ;; (message-box "%s" candidate)
                    ))))

(defun klin-pdf-helm-browse-pdf-buffers ()
  "Use helm to quickly locate pdf buffers."
  (interactive)
  (helm :sources '(klin-helm-source-make-pdfs-visible)))

(defun open-pdf-document-new-frame (&optional filepath page)
  "Open the pdf file at FILEPATH in a new frame on a certain PAGE."
  (unless page (setq page 1))
  (let* ((new-frame (make-frame)))
    (with-selected-frame new-frame
      (find-file-existing filepath)
      (pdf-view-goto-page page))
    new-frame))

(defun open-pdf-document-other-frame-or-window
    (filepath page &optional maximize frame-or-window)
  "Open the pdf file at FILEPATH in another window and on a certain PAGE.
by default, the pdf's window's frame will be maximized.
You can set MAXIMIZE be a non-nil value to maximize the window.
FRAME-OR-WINDOW < 0 -> split window
FRAME-OR-WINDOW > 0 -> frame
FRAME-OR-WINDOW = nil -> find-file"
  (unless page (setq page 1))
  (cond
   ((not frame-or-window)
    (find-file filepath))
   ((< frame-or-window 0)
    (progn

      ;; FIXME: this worked once, but after relaunching emacs didn't
      ;; (if winner-mode
      ;;     (winner-save-unconditionally))

      (split-window-below)
      (other-window 1)
      (window-resize (selected-window)
                     (round (* (- 1 (/ 1 1.68)) (window-height))))
      (find-file filepath)
      (if (eq major-mode 'pdf-view-mode)
          (pdf-view-redisplay))))
   ((> frame-or-window 0)
    (find-file-other-frame filepath)))

  (pdf-view-goto-page page)
  ;; if it's not fullscreen, make it fullscreen with
  ;; frame.el's toggle-frame-fullscreen, which keeps
  ;; the previous frame dimensions stored so that
  ;; you can toggle back
  (if maximize
      (let* ((fullscreen (frame-parameter nil 'fullscreen)))
        (unless fullscreen
          (toggle-frame-maximized))))
  (pdf-view-redisplay))

;; (defun pdf-redisplay-on-window-size-change ()
;;   "Re-displays pdfview on window size change.
;; E.g. re-centers it.
;; Assumes pdf-tools is used to view pdfs."
;;   ;; for some reason pdf-view-mode is not defined
;;   ;; as a variable, so check for another pdf-tools
;;   ;; associated buffer-local minor mode, pdf-outline-minor-mode
;;   ;; see also (pdf-tools-modes)
;;   (if (bound-and-true-p pdf-outline-minor-mode)
;;       (pdf-view-redisplay)))

;; (setq window-size-change-functions nil)

;; (add-hook 'pdf-outline-minor-mode-hook
;;           (lambda ()
;;             (add-to-list 'window-size-change-functions
;;                          'pdf-redisplay-on-window-size-change)))

(defun klin-pdf-make-pdf-frame-invisible ()
  "Make the current frame invisible.
Intended for PDF frame management."
  (interactive)
  (if (string-equal (file-name-extension (buffer-file-name)) "pdf")
      (make-frame-invisible (selected-frame)))
  ;; (make-frame-invisible (window-frame (get-buffer-window (current-buffer) t)))
  )

(defun klin-pdf-make-pdf-frame-visible (&optional bufname)
  "Make frame with a certain BUFNAME in it visible."
  (interactive)
  (let* ((buffer (get-buffer bufname))
         (bufwindow (get-buffer-window buffer t)))
    (if bufwindow
        (progn
          (make-frame-visible (window-frame bufwindow))
          (x-focus-frame (window-frame bufwindow)))

      ;; (setq newframe (make-frame))
      ;; (select-frame newframe)
      ;; (when (display-graphic-p frame)
      ;; (switch-to-buffer buffer)
      (switch-to-buffer-other-frame bufname)
      ;; (message (concat "current buffer: " (buffer-name (current-buffer))))
      (pdf-view-redisplay) ;; That fixed the raw-pdf "fundamentalmode" stalling for me in emacs 25.2.2 and pdf-tools 1.0
      ;; (message (concat "i tried pdf-view-redisplay"))
 )))

(provide 'klin-pdf-frames)
;;; klin-pdf-frames.el ends here
