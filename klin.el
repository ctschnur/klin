;;; klin.el --- key bindings for klin functions in different modes  -*- lexical-binding: t; -*-

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

;; Key bindings can either be (1) declared in a new map bundled within a
;; minor mode (first option: buffer-local, second option global),
;; or (2) they can be (easier option in my opinion) just set in hooks
;; when entering the specific file type or mode (e.g. org-mode) where
;; you want to have them available.  I'm going to do (2) here.

;;; Code:

;; (require 'frame-cmds)
;; (add-to-list 'load-path (expand-file-name (file-name-directory (buffer-file-name))))

(require 'klin-utils)
(require 'klin-org)
(require 'klin-bibtex)

(require 'klin-optional)

;; (require 'tabbar)
;; (tabbar-mode 1)
(require 'klin-tabs)
(require 'klin-pdf-frames)

;; keys from within org-mode
(require 'org)
;; (define-key org-mode-map (kbd "C-M-, o") 'klin-org-open-link-nearest-to-point)

;; multiple-cursors operates only reliably in the current frame
;; so you have to change back to the current frame after
;; opening up the n-m th frame in a series of n new frames
(define-key org-mode-map (kbd "C-M-, o") (lambda ()
                                           (interactive)
                                           (let* ((orig-window (selected-window))
                                                  (orig-buffer (current-buffer))
                                                  (orig-frame (selected-frame)))
                                             (klin-org-open-link-nearest-to-point)
                                             (select-window orig-window))))

;; (define-key org-mode-map (kbd "C-M-, o") (lambda ()
;;                                            (interactive)
;;                                            (message "hi")))

(define-key org-mode-map (kbd "C-M-, l") 'klin-org-insert-pdf-link)

;; keys from within pdf-view-mode
(require 'pdf-view)
(define-key pdf-view-mode-map (kbd "C-M-, l") 'klin-pdf-pdfview-store-link)
(define-key pdf-view-mode-map (kbd "C-M-, i") 'klin-pdf-make-pdf-frame-invisible)

;; keys for global pdf operations
;; - visibility/rearranging
;; (global-set-key (kbd "C-M-, I") 'make-pdf-frames-invisible-all)
;; (global-set-key (kbd "C-M-, V") 'make-pdf-frames-visible-all)

;; - browsing open, opening, and closing open pdfs
(global-set-key (kbd "C-M-, p") 'klin-pdf-helm-browse-pdf-buffers)
(global-set-key (kbd "C-M-, P") 'klin-tabs-open-pdfs)
(global-set-key (kbd "C-M-, k") 'kill-frame-and-buffers-within)

;; keys for global bibfile adding/checking operations
(global-set-key (kbd "C-M-, m") 'klin-bibtex-set-bibfiles-for-pdfs)

;; within bibtex buffers
(define-key bibtex-mode-map (kbd "C-M-, c") 'klin-bibtex-compare-entry-to-original-bibfile)
(define-key bibtex-mode-map (kbd "C-M-, o") 'klin-open-pdf-from-bibtex)

;; jump to citation in bibtex buffer
(define-key org-mode-map (kbd "C-M-, j") 'org-ref-open-citation-at-point)

(provide 'klin)
;;; klin.el ends here
