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
;; along with this program.  If not, see <ht


;;; Code:

;; -------- detach window to somewhere

;; (defvar klin-detatched-buffer nil
;;   "This holds the detatched window's buffer.")

;; (defun klin-detatch-buffer-close-window (&optional ARG)
;;   "Copy a window's buffer and close the window.
;; Then, it is ready to be inserted somewhere else.
;; With non-nil ARG, the window should be popped up as a new frame."
;;   (interactive)

;;   )

;; ------------ pop buffer to new next tab
(defun copy-buffer-to-new-next-tab ()
  "Create a new elscreen screen to the right of the current one and open buffer."
  (interactive)
  (let* ((buf (current-buffer))
         (original-screen-pos (elscreen-get-current-screen)))
    (elscreen-create)
    (switch-to-buffer buf)
    (elscreen-move-tab-to-position (+ original-screen-pos
                                      1)))
  )
;; ------------
;; ------------ pop buffer into new frame
(defun pop-buffer-into-new-frame ()
  "Pop buffer into new frame."
  (let* ((this-frame (selected-frame))
         (buf (current-buffer))
         (this-window (selected-window))
         (new-frame (make-frame)))
    (with-selected-frame new-frame
      (switch-to-buffer buf))
    (with-selected-frame this-frame
      (delete-window this-window))))
;; ------------
;; ------------ pop buffer into new elscreen tab in other frame
(defun pop-buffer-into-new-elscreen-tab-in-other-frame ()
  "TODO: not finished.
Pop buffer into new elscreen tab in other frame."
    (defvar helm-source-pop-into-new-elscreen-tab-in-other-frame '((name . "jump to frame:")
                                                       (candidates . klin-bibtex-get-collective-bibtex-files)
                                                       (action . (lambda (candidate)
                                                                   (klin-org-action-jump-to-collective candidate)))))
  )
;; ------------
;; --------

(defun elscreen-get-next-higher-screen-pos ()
  (seq-min (remove nil
                   (mapcar (lambda (elem)
                             (if (> elem (elscreen-get-current-screen))
                                 elem))
                           (elscreen-get-screen-list)))))

(defun elscreen-get-next-lower-screen-pos ()
  (seq-max (remove nil
                   (mapcar (lambda (elem)
                             (if (< elem (elscreen-get-current-screen))
                                 elem))
                           (elscreen-get-screen-list)))))

(defun elscreen-move-tab-right ()
  "Move tab in elscreen."
  (interactive)
  (elscreen-next)
  (elscreen-swap))

(defun elscreen-move-tab-left ()
  "Move tab in elscreen."
  (interactive)
  (elscreen-previous)
  (elscreen-swap))

(defun elscreen-move-tab-to-position (target-pos)
  "Move tab to position TARGET-POS, displacing other tabs."
  (interactive)
  ;; clamp
  (cond
   ((< target-pos (seq-min (elscreen-get-screen-list)))
    (setq target-pos (seq-min (elscreen-get-screen-list))))
   ((> target-pos (seq-max (elscreen-get-screen-list)))
    (setq target-pos (seq-max (elscreen-get-screen-list)))))
  ;; swap through
  (while (/= target-pos (elscreen-get-current-screen))
    (cond
     ((< target-pos (elscreen-get-current-screen)) (elscreen-move-tab-left))
     ((> target-pos (elscreen-get-current-screen)) (elscreen-move-tab-right)))))


(defun klin-tabs-open-pdfs-in-new-frame (pdf-list)
  "Open PDF-LIST in chrome-like tabs with elscreen.
Ootb limited to only 10 tabs, but probably configurable."
  (interactive)
  (let* ((ctr 0)
         frame)

    (setq frame (make-frame))

    (with-selected-frame frame
      (while (< ctr (length pdf-list))
        (elscreen-create)
        (find-file (nth ctr pdf-list))
        (setq ctr (+ 1 ctr))
        (pdf-view-fit-page-to-window)))
    pdf-list))

(defun klin-tabs-open-pdfs (&optional arg)
  "Open pdfs from Library in new frame."
  (interactive)
  (klin-tabs-open-pdfs-in-new-frame
   (helm-read-file-name "Select pdf: "
                        :initial-input
                        (concat (expand-file-name pdf-library-dir)
                                " .pdf$ ")
                        :marked-candidates t)))

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
