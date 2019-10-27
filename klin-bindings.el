;;; klin-bindings.el --- declare key-bindings here   -*- lexical-binding: t; -*-

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


(require 'klin-utils)
(require 'klin-org)
(require 'klin-bibtex)
(require 'klin-optional)
(require 'klin-tabs)
(require 'klin-pdf-frames)
(require 'klin-presentations)

(require 'org)
(require 'hydra)
(require 'pdf-view)

;; -------- org-mode important klin keys
(define-key org-mode-map (kbd "C-M-, l") 'klin-org-insert-pdf-link)
(define-key org-mode-map (kbd "C-M-, o") 'klin-org-open-link-nearest-to-point)
(define-key org-mode-map (kbd "C-M-, b j") 'org-ref-open-citation-at-point)
(define-key org-mode-map (kbd "C-M-, w") 'klin-org-watch-and-insert-scanned-file)
(define-key org-mode-map (kbd "C-M-, b c") 'klin-bibtex-jump-to-collective-bib-file)

(define-key org-mode-map (kbd "C-M-, p") ; p: process
  (defhydra hydra-klin-from-org (:columns 3)
    "klin: process in org"
    ("i l o" (lambda ()
           (interactive)
           (klin-org-insert-latex-overhead)) "insert LaTeX overhead")))

;; ---------- presentations with org-mode, latex beamer and inkscape
(define-key org-mode-map (kbd "C-M-, i") 'klin-open-in-inkscape)
;; ----------

;; --------

;; ------- within a normal pdf
(define-key pdf-view-mode-map (kbd "C-M-, l") 'klin-pdf-pdfview-store-link)
;; -------

;; ------- within a bibtex buffer
(define-key bibtex-mode-map (kbd "C-M-, e o p") 'klin-bibtex-entry-open-pdf)
(define-key bibtex-mode-map (kbd "C-M-, g t i") 'klin-bibtex-grab-template-isbn)
(define-key bibtex-mode-map (kbd "C-M-, e f f") 'klin-bibtex-entry-fix-filepath-file-page-offset)   ; fix entry file-
(define-key bibtex-mode-map (kbd "C-M-, e i c") 'klin-bibtex-integrate-bib-entry-into-collective)
(define-key bibtex-mode-map (kbd "C-M-, o p") 'klin-bibtex-open-pdf-from-bibtex)

;; process next/previous pdf
;; (define-key bibtex-mode-map (kbd "C-M-, p n") 'klin-bibtex-process-next-pdf)
;; (define-key bibtex-mode-map (kbd "C-M-, p n") 'klin-bibtex-process-previous-pdf)

;; (global-set-key (kbd "C-M-,") nil)
(define-key bibtex-mode-map (kbd "C-M-, p")
  (defhydra hydra-klin-from-bibtex (:columns 3)
    "klin in bibtex"
    ("o p c" (lambda ()
           (interactive)
           (klin-bibtex-process-open-current-pdf)) "open pdf")
    ("o p b"
      (lambda ()
        (interactive)
        (klin-bibtex-open-pdf-from-bibtex))
      "open pdf")
    ("n" (lambda ()
           (interactive)
           (klin-bibtex-process-next-pdf)) "next")
    ("p" (lambda ()
           (interactive)
           (klin-bibtex-process-previous-pdf)) "previous")
    ("g t i" (lambda ()
           (interactive)
           (klin-bibtex-grab-template-isbn)) "grab template from ISBN")
    ("f p" (lambda ()
             (interactive)
             (klin-bibtex-entry-fix-filepath-field)) "fix file-path")
    ("f o" (lambda ()
             (interactive)
             (klin-bibtex-entry-fix-file-page-offset)) "fix file-page-offset")
    ("c o" (lambda ()
           (interactive)
           (klin-bibtex-compare-entry-to-original-bibfile))
     "comp. coll[..].bib, self.bib")
    ("e i c" (lambda ()
               (interactive)
               (klin-bibtex-integrate-bib-entry-into-collective)) "integrate into collective")
    ("F" (lambda ()
           (interactive)
           (klin-bibtex-process-finish-current-pdf)) "finish current")
    ("c l" (lambda ()
           (interactive)
           (klin-bibtex-process-clean-whole-list)) "clean out list")
    ("q" (lambda ()
           (interactive)
           (if (y-or-n-p (format "Close this frame? "))
               (delete-frame)
             (message "Not closing frame"))) "quit frame")
    ))

;; ----------

;; -------- globally important klin keys
;; browsing open, opening, and closing open pdfs
(global-set-key (kbd "C-. p") 'klin-pdf-helm-browse-pdf-buffers)
(global-set-key (kbd "C-. P") 'klin-tabs-open-pdfs)
(global-set-key (kbd "C-. b") 'klin-bibtex-set-bibfiles-for-pdfs)
                                        ;
; (global-set-key (kbd "C-M-, k") 'kill-frame-and-buffers-within)

(provide 'klin-bindings)


;;; klin-bindings.el ends here
