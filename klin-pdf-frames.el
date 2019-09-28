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

;;

;;; Code:

(require 'helm)
(require 'pdf-view)


(defun get-all-pdf-buffers ()
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


(defvar some-helm-source
      '((name . "make visible and bring to front PDF buffer(s)")
        (candidates . get-all-pdf-buffers)
        (action . (lambda (candidate)
                    (make-visible candidate)
                    ;; (message-box "%s" candidate)
                    ))))

(make-variable-buffer-local 'some-helm-source)

(defun helm-browse-pdf-buffers ()
  "Use helm to quickly locate pdf buffers."
  (interactive)
  (helm :sources '(some-helm-source)))

(defun open-pdf-document-new-frame (&optional filepath page)
  "Open the pdf file at FILEPATH in a new frame on a certain PAGE."
  (unless page (setq page 1))
  (unless filepath (setq filepath (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  (progn
    (find-file-other-frame filepath)
    (pdf-view-goto-page page)))

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

(provide 'klin-pdf-frames)
;;; klin-pdf-frames.el ends here
