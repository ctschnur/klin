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
(require 'klin-tabs)
(require 'klin-pdf-frames)

(require 'org)
(require 'hydra)


;; (define-key org-mode-map (kbd "C-M-, o") (lambda ()  ; multiple-cursors works ootb only with lambdas
;;                                            (interactive)
;;                                            (let* ((orig-window (selected-window))
;;                                                   (orig-buffer (current-buffer))
;;                                                   (orig-frame (selected-frame)))
;;                                              (klin-org-open-link-nearest-to-point)
;;                                              ;; multiple-cursors operates only reliably in the current frame
;;                                              ;; so you have to change back to the current frame after
;;                                              ;; opening up the n-m th frame in a series of n new frames
;;                                              (select-window orig-window))))

(require 'pdf-view)
;; (define-key org-mode-map (kbd "C-M-, l") 'klin-org-insert-pdf-link)
;; (define-key pdf-view-mode-map (kbd "C-M-, l") 'klin-pdf-pdfview-store-link)

;; -------- org-mode important klin keys
;; org-mode klin hydra
(define-key org-mode-map (kbd "C-M-,")
  (defhydra hydra-klin-org-mode-buffer-menu ()
    "klin in org"
    ("o" (lambda () ; multiple-cursors works ootb only with lambdas
           (interactive)
           (let* ((orig-window (selected-window))
                  (orig-buffer (current-buffer))
                  (orig-frame (selected-frame)))
             (klin-org-open-link-nearest-to-point)
             ;; multiple-cursors operates only reliably in the current frame
             ;; so you have to change back to the current frame after
             ;; opening up the n-m th frame in a series of n new frames
             (select-window orig-window))) "open pdf link (klin)")
    ("l" (lambda ()
           (interactive)
           (klin-org-insert-pdf-link)) "insert pdf link (klin)")
    ("j" (lambda ()
           (interactive)
           (org-ref-open-citation-at-point)) "open citation at point")
    ("w" (lambda ()
           (interactive)
           (klin-org-watch-and-insert-scanned-file)) "watch for scan coming in")))
;; --------

;; -------- globally important klin keys
;; browsing open, opening, and closing open pdfs
;; (global-set-key (kbd "C-M-, p") 'klin-pdf-helm-browse-pdf-buffers)
;; (global-set-key (kbd "C-M-, P") 'klin-tabs-open-pdfs)
;; (global-set-key (kbd "C-M-, k") 'kill-frame-and-buffers-within)

;; repeat them in a hydra
(global-set-key (kbd "C-M-,")
  (defhydra hydra-klin-from-anywhere ()
    "klin in general"
    ("p" (lambda ()
           (interactive)
           (klin-pdf-helm-browse-pdf-buffers)) "browse pdf buffers")
    ("P" (lambda ()
           (interactive)
           (klin-tabs-open-pdfs)) "open selection of pdfs in tabs")
    ("b" (lambda ()
           (interactive)
           (klin-bibtex-set-bibfiles-for-pdfs)) "add/edit pdf assoc. bib files")
    ("k" (lambda ()
           (interactive)
           (kill-frame-and-buffers-within)) "kill frame and buffers")))
;; -----------

;; ----------- within bibtex buffers
(define-key bibtex-mode-map (kbd "C-M-,")
  (defhydra hydra-klin-from-bibtex ()
    "klin in bibtex"
    ("i" (lambda ()
           (interactive)
           (klin-bibtex-entry-fill-isbn-manually))
     "isbn download entry")
    ("f" (lambda ()
           (interactive)
           (klin-bibtex-entry-fill-filepath-and-file-page-offset-manually))
     "fix filepath and file-page-offset")
    ("c" (lambda ()
           (interactive)
           (klin-bibtex-compare-entry-to-original-bibfile))
     "compare entry (collective <-> pdf-assoc) bibfile")
    ("p" (lambda ()
           (interactive)
           (klin-open-pdf-from-bibtex))
     "open pdf")))
;; ----------

;; --------- pdf-view-mode to make pdf pinch/zoom more chrome-like
(define-key pdf-view-mode-map (kbd "<S-mouse-5>") 'image-forward-hscroll)
(define-key pdf-view-mode-map (kbd "<S-mouse-4>") 'image-backward-hscroll)
(define-key pdf-view-mode-map (kbd "<C-mouse-5>") (lambda () (interactive) (pdf-view-enlarge 1.1)))
(define-key pdf-view-mode-map (kbd "<C-mouse-4>") (lambda () (interactive) (pdf-view-shrink 1.1)))
;; ---------

(provide 'klin)
;;; klin.el ends here
