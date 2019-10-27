;;; klin-hydras.el --- if you use hydras, define them here  -*- lexical-binding: t; -*-

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

(require 'org)
(require 'hydra)
(require 'pdf-view)

;; org-mode klin hydra



;; (defun re-def-hydra-klin-org ()
;;   "Redefine it if you change to `org-mode' or to `org-noter-doc-mode'."
;;   (interactive)
;;   (let* (result
;;          (org-mode-standard-hydra
;;           '(defhydra hydra-klin-org (:columns 2)
;;             "klin in org"
;;             ("o"
;;              (lambda () ; multiple-cursors works ootb only with lambdas
;;                (interactive)
;;                (let* ((orig-window (selected-window))
;;                       (orig-buffer (current-buffer))
;;                       (orig-frame (selected-frame)))
;;                  (klin-org-open-link-nearest-to-point)
;;                  ;; multiple-cursors operates only reliably in the current frame
;;                  ;; so you have to change back to the current frame after
;;                  ;; opening up the n-m th frame in a series of n new frames
;;                  (select-window orig-window)))
;;              "open pdf link (klin)")
;;             ("l"
;;              (lambda ()
;;                (interactive)
;;                (klin-org-insert-pdf-link))
;;              "insert pdf link (klin)")
;;             ("j"
;;              (lambda ()
;;                (interactive)
;;                (org-ref-open-citation-at-point))
;;              "open citation at point")
;;             ("w"
;;              (lambda ()
;;                (interactive)
;;                (klin-org-watch-and-insert-scanned-file))
;;              "watch for scan coming in")
;;             ("?"
;;              (lambda ()
;;                (interactive))) ; do nothing, just show hint
;;             ))

;;          (org-noter-additional-hydra
;;           '(defhydra+ hydra-klin-org ()
;;              "klin in org-noter"
;;              ("n"
;;              (lambda ()
;;                (interactive)
;;                (org-noter))
;;              "run org-noter")
;;             ("i"
;;              (lambda ()
;;                (interactive)
;;                (with-selected-window (org-noter--get-doc-window)
;;                  ;; (select-window (org-noter--get-doc-window))
;;                  (org-noter-insert-note)))
;;              "insert note (page)")
;;             ("p"
;;              (lambda ()
;;                (interactive)
;;                (with-selected-window (org-noter--get-doc-window)
;;                  ;; (select-window (org-noter--get-doc-window))
;;                  (org-noter-insert-precise-note)))
;;              "insert note (precise)"))))

;;     (eval
;;      (remove nil `(progn
;;                     ,(if (eq major-mode 'org-mode)
;;                          org-mode-standard-hydra)
;;                     ,(if org-noter-notes-mode   ; if in that minor mode, this is
;;                          org-noter-additional-hydra)))

;;      )))

;; (define-key org-mode-map (kbd "C-M-,") (lambda ()
;;                                          (interactive)
;;                                          (funcall (re-def-hydra-klin-org))))

;; --------- pdf-view-mode hydra

;; (defun re-def-hydra-klin-pdf-view ()
;;   "Redefine it"
;;   (interactive)
;;   (let* ((pdf-view-mode-standard-hydra
;;           '(defhydra hydra-klin-pdf-view ()
;;             "klin in pdf-view"
;;             ("g"
;;              (lambda ()
;;                (interactive)
;;                (call-interactively 'pdf-view-goto-page))
;;              "go to page")
;;             ("n"
;;              (lambda ()
;;                (interactive)
;;                (org-noter))
;;              "run org-noter")
;;             ("?"
;;              (lambda ()
;;                (interactive)
;;                )) ; do nothing, just show hint
;;             ))

;;          (pdf-view-mode-org-noter-additional-hydra
;;           '(defhydra+ hydra-klin-pdf-view ()
;;             "klin in org-noter (doc)"
;;             ("n"
;;              (lambda ()
;;                (interactive)
;;                (org-noter-insert-note))
;;              "make note (page)")
;;             ("p"
;;              (lambda ()
;;                (interactive)
;;                (org-noter-insert-precise-note))
;;              "make note (precise)"))))

;;     (eval
;;      (remove nil
;;              `(progn
;;                 ,(if (eq major-mode 'pdf-view-mode)
;;                      pdf-view-mode-standard-hydra)
;;                 ,(if org-noter-doc-mode            ; if in that minor mode, this is
;;                      pdf-view-mode-org-noter-additional-hydra)))
;;      )))


;; (define-key pdf-view-mode-map (kbd "C-M-,") (lambda ()
;;                                          (interactive)
;;                                          (funcall (re-def-hydra-klin-org))))
;; ---------

;; (add-hook 'org-mode-hook 're-def-hydra-klin-org)
;; (add-hook 'org-noter-notes-mode-hook 're-def-hydra-klin-org)


;; --------- globally important klin keys
;; repeat them in a hydra
;; (global-set-key (kbd "C-M-.")
;;   (defhydra hydra-klin-from-anywhere ()
;;     "klin in general"
;;     ("p" (lambda ()
;;            (interactive)
;;            (klin-pdf-helm-browse-pdf-buffers)) "browse pdf buffers")
;;     ("P" (lambda ()
;;            (interactive)
;;            (klin-tabs-open-pdfs)) "open selection of pdfs in tabs")
;;     ("b" (lambda ()
;;            (interactive)
;;            (klin-bibtex-set-bibfiles-for-pdfs)) "add/edit pdf assoc. bib files")
;;     ("k" (lambda ()
;;            (interactive)
;;            (kill-frame-and-buffers-within)) "kill frame and buffers")))
;; ----------

;; ----------- within bibtex buffers
;; (define-key bibtex-mode-map (kbd "C-M-,")
;;   (defhydra hydra-klin-from-bibtex ()
;;     "klin in bibtex"
;;     ("i" (lambda ()
;;            (interactive)
;;            (klin-bibtex-entry-open-pdf))
;;      "isbn download entry")
;;     ("f" (lambda ()
;;            (interactive)
;;            (klin-bibtex-entry-fix-filepath-file-page-offset))
;;      "fix filepath and file-page-offset")
;;     ("c" (lambda ()
;;            (interactive)
;;            (klin-bibtex-compare-entry-to-original-bibfile))
;;      "compare entry (collective <-> pdf-assoc) bibfile")
;;     ("p" (lambda ()
;;            (interactive)
;;            (klin-bibtex-open-pdf-from-bibtex))
;;      "open pdf")))
;; ----------


(provide 'klin-hydras)
;;; klin-hydras.el ends here
