;;; klin-tabs.el --- Tab handling for chrome-like pdf browsing  -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; --------- tabbar simple grouping customization

(defun my-tabbar-buffer-groups-standard ()
  "Return the name of the tab group names the current buffer belongs to.
There are two groups: Emacs buffers (those whose name starts with '*', plus
dired buffers), and the rest.  This works at least with Emacs v24.2 using
tabbar.el v1.7."
  (list (cond
         ((string-equal "*"
                        (substring (buffer-name)
                                   0
                                   1)) "emacs")
         ((and (string-match "pdf$" (buffer-file-name))
               (not (string-match "main\\.pdf$" (buffer-file-name)))) "pdf")
         ((eq major-mode 'dired-mode) "emacs")
         (t "user"))))

;; assign this to gouvern the behaviour of tabbar
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups-standard)

;; -----------

(defun klin-tabs-open-pdfs ()
  "Open a pdf from Library."
  (interactive)
  (let* ((pdf-list (helm-read-file-name
                     "Select pdf: "
                     :initial-input (concat
                                     (expand-file-name "~/Dropbox/2TextBooks")
                                     "/ .pdf$ ")
                     :marked-candidates t))
         frame
         k-p-f)
    (mapcar
     (lambda (pdf-path)
       (let* (
              ;;(tab-buffers)
              )
         ;; open them freshly in a new frame with a new ordered tab-buffers list
         (if (not frame)
             (progn
               (setq frame (progn
                             (find-file-other-frame pdf-path)
                             (selected-frame)))
               ;; (add-to-list 'klin-pdfs-frames
               ;;              (setq k-p-f
               ;;                    (make-klin-pdfs-frame :frame frame
               ;;                                          :tab-buffers nil)))
               ))
         (x-focus-frame frame)
         (find-file pdf-path)
         ;; add that pdf-path to the klin-pdf-frame's (k-p-f's) tab-buffers list
         (setf (klin-pdfs-frame-tab-buffers k-p-f)
               (append (klin-pdfs-frame-tab-buffers k-p-f) `(,(current-buffer))))

         ;; (add-to-list 'klin-pdfs-frames k-p-f)
         ;; (split-window-horizontally)
         (maximize-frame)
         (pdf-view-fit-page-to-window)))
     pdf-list)))

;; if I have a pdf and open it with find-file, I'd like to
;; clone it if it's already open somewhere and give it another buffer
;; (only if tabbar-mode is enabled)

;; TODO: now only find a way to run this
;; (since find-file-hook actually doesn't run if the file is already open)
(defun klin-clone-pdf-buffer-instead-of-find-file ()
  "Run this on `find-file-hook' to make a pdf clone if it's already there."
  (interactive)
  (when (and (stringp buffer-file-name)
             (string-match "pdf$" buffer-file-name)
             (member buffer-file-name
                     (mapcar (lambda (buf)
                               (with-current-buffer (buffer-file-name)))
                             (buffer-list))))
    (message "clone it")))

;; (add-hook 'find-file-hook 'klin-clone-pdf-buffer-instead-of-find-file)
;; (remove-hook 'find-file-hook 'klin-clone-pdf-buffer-instead-of-find-file)

;; (defun klin-my-tabbar-buffer-groups ()
;;   "For every new pdf buffer that is opened, run this function.
;; - If it's just one pdf: create a unique-id.
;;   - search for buffers that are clones of the same file (end with <integer>)
;;     and create a unique clones-id (integer number)
;;     and a clones-base-id (integer number to link to the original file, 0th clone)
;; - If it's several pdfs: create a unique-id for each, and one group-greater-1-id")

;; (setq tabbar-buffer-groups-function 'klin-my-tabbar-buffer-groups)

;; in tabbar.el, there is a problem that inside one tabbar, you can't have the same buffer
;; more than once. But I would like that feature, since e.g. in chrome you can show the same
;; pdf more than once

;; when having multiple cloned buffers of one pdf exist at the
;; same time, they should all automatically revert whenever the pdf
;; is edited in one of them (anntoation, hightlight, etc.)

(defun klin-clone-pdf-buffer ()
  "Clone a pdf buffer to enable multiple tabbar tabs for one file.
Make pdf-view-mode with pdf-tools by default auto-revert pdf file buffers
without asking.  The problem is that in buffer clones, new annotations
aren't immediately shown after reverting.
Calling `pdf-view-mode' (or `normal-mode') makes those changes visible,
but changes the zoom and goes to the first page of the pdf.
Actually, this would all be very easy if Emacs had the capability to
create a tab bar of different windows.  A different window with the
same buffer will always update that buffer, no matter what."
  (interactive)
  ;; Try to find another function that accomplishes the refresh
  ;; but preserves the scroll state
  (let* ((orig-file-name (buffer-file-name))
         (orig-buffer (current-buffer))
         (bmk-record (pdf-view-bookmark-make-record))
         (clone-buffer (clone-indirect-buffer nil t))
         )
    ;; for some reason, the buffer-file-name is nil
    ;; if you make an indirect clone.
    ;; So, set the buffer-file-name to the original one.
    ;; setup the clone buffer
    (with-current-buffer clone-buffer
      (setq buffer-file-name orig-file-name)
      ;; refresh it so that it shows new annotations and stuff
      (pdf-view-mode)
      ;; (normal-mode)
      ;; if you add annotations, you should immediateley save them
      ;; if you want them to show up
      (revert-buffer nil t)
      (pdf-view-bookmark-jump bmk-record))

    (current-buffer)
    (delete-window (selected-window)) ;; goes back to last window
    ;; (other-window-or-frame -1)
      ;; (switch-to-buffer clone-buffer)

    ;; (switch-to-buffer clone-buffer)
    ;; now do the refreshing thing with normal mode, etc...
    )
  )

;; (defun update-pdf-view-buffer-on-buffer-list-update-hook ()
;;   "Update the pdf to include all annotations created in cloned buffer.
;; Every time you visit a new buffer, check it's mode for pdf-view-mode.
;; If it's the case, run the function."
;;   (if (not (active-minibuffer-window))
;;       (let* ((forefront-buffer (car (buffer-list))))
;;         (if (eq (with-current-buffer forefront-buffer
;;                   major-mode)
;;                 'pdf-view-mode)
;;           (let* ((bmk-record (pdf-view-bookmark-make-record)))
;;               (pdf-view-mode)
;;               (pdf-view-bookmark-jump bmk-record))))))

;; if you switch to a pdf buffer, update it (it could be a clone)
;; (add-hook 'buffer-list-update-hook
;;          'update-pdf-view-buffer-on-buffer-list-update-hook)

;; (delete-hook 'buffer-list-update-hook
;;             'update-pdf-view-buffer-on-buffer-list-update-hook)

(defun refresh-pdf-view-clone ()
  "Refresh the pdf, so that the annotations in clones are shown."
  (let* ((bmk-record (pdf-view-bookmark-make-record)))
    (pdf-view-mode)
    (pdf-view-bookmark-jump bmk-record)))


(provide 'klin-tabs)
;;; klin-tabs.el ends here
