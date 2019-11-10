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


;; -------- within org-noter ---------
(with-eval-after-load 'org-noter
  (define-key org-noter-notes-mode-map (kbd "C-S-M-p") 'org-noter-sync-prev-note-in-prev-pdf)
  (define-key org-noter-notes-mode-map (kbd "C-S-M-n") 'org-noter-sync-next-note-in-next-pdf)
  (define-key org-noter-notes-mode-map (kbd "C-S-M-h") 'org-noter-switch-to-base-buffer))

(define-key org-mode-map (kbd "C-S-M-l") 'org-noter)

;; ----------

;; -------- org-mode important klin keys
(define-key org-mode-map (kbd "C-M-, l") 'klin-org-insert-pdf-link)
;; (define-key org-mode-map (kbd "C-M-, o") 'klin-org-open-link-nearest-to-point)
(define-key org-mode-map (kbd "C-M-, b j") 'org-ref-open-citation-at-point)
(define-key org-mode-map (kbd "C-M-, w") 'klin-org-watch-and-insert-scanned-file)
(define-key org-mode-map (kbd "C-M-, b c") 'klin-bibtex-jump-to-collective-bib-file)


(defun link-type-at-point ()
  (interactive)
  (let* ((link-type (org-element-property :type (org-element-context))))
    link-type))

(defun my-open-freehand-generated-pdf (link-content)
  "Check if a pdf for the link-content with the appropriate extensions exists."
  (interactive)
  (let* ((link-content (org-element-property :path (org-element-context)))
         (freehand-notes-file-path (my-get-file-from-searchpaths link-content))
         (assoc-pdf-file-path (my-get-freehand-note-filepath-associated-pdf-filepath link-content)))
    (if assoc-pdf-file-path
        (progn
          (open-pdf-document-other-frame-or-window assoc-pdf-file-path 1 nil -1)
          (auto-revert-mode 1))
      (user-error "No such pdf exists"))))

(defun org-run-context-aware-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-open-from-org
                                      (:columns 3 :exit t)
                                      "klin: open from org"
                                      ,(let* ((link-content (org-element-property :path (org-element-context))))
                                         (if (string-equal (link-type-at-point)
                                                           "freehand")
                                             '("o"
                                               (lambda ()
                                                 (interactive)
                                                 (my-open-freehand-notes-assoc-pdf))
                                               "Open pdf (generated from freehand note)")
                                           (if (or (string-equal (link-type-at-point)
                                                                 "cite")
                                                   (string-equal (link-type-at-point)
                                                                 "file"))
                                               '("o"
                                                 (lambda ()
                                                   (interactive)
                                                   (klin-org-open-link-nearest-to-point))
                                                 "Open pdf"))))
                                      ,(if (string-equal (link-type-at-point)
                                                         "freehand")
                                           '("f"
                                             (lambda ()
                                               (interactive)
                                               (my-run-freehand-notes-program-with-file
                                                (org-element-property :path (org-element-context))))
                                             "Open freehand program"))
                                      ("b p"
                                       (lambda ()
                                         (interactive)
                                         (klin-org-open-bibliographys-pdfs))
                                       "Open bibliography PDFs")
                                      ("q" nil "cancel"))))))
    ;; (hydra-klin-open-from-org/body)
    ;; (boundp 'hydra-klin-open-from-org/body)
    ;; (when (boundp 'hydra-klin-open-from-org/body)
    ;;   (hydra-klin-open-from-org/body))
    ;; hydra-body
    ;; hydra-klin-open-from-org/body
    ;; (boundp 'hydra-klin-open-from-org/body)
    ;; (bound-and-true-p hydra-klin-open-from-org/body)
    ;; (boundp 'hydra-body)
    ;; (bound-and-true-p hydra-body)
    ;; (fmakunbound 'hydra-body)

    (hydra-klin-open-from-org/body)
    (fmakunbound 'hydra-klin-open-from-org/body)
    (setq hydra-klin-open-from-org/body nil)))

(define-key org-mode-map (kbd "C-M-, o") ; o: open
  'org-run-context-aware-hydra)

(defun pdf-view-run-context-aware-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-open-from-pdf-view
                                      (:columns 3 :exit t)
                                      "klin: open from pdf-view"
                                      ;; if generating freehand notes exist
                                      ,(if (file-exists-p (concat (file-name-sans-extension (buffer-file-name))
                                                                  "."
                                                                  my-freehand-notes-extension))
                                           '("f g"
                                             (lambda ()
                                               (interactive)
                                               (auto-revert-mode 1)
                                               (my-run-freehand-notes-program-with-file))
                                             "open generating freehand notes"))
                                      ;; if annotating freehand notes exist
                                      ,(let* ((link-content-from-pdf-view (file-name-base (buffer-file-name)))
                                              )
                                         (if (file-exists-p (concat (my-get-freehand-note-annotating-filename link-content-from-pdf-view)))
                                             (let* ((searched-for-link-content (concat link-content-from-pdf-view
                                                                                       my-annotating-freehand-notes-filename-tag)))
                                               `("f a"
                                                 (lambda ()
                                                   (interactive)
                                                   (auto-revert-mode 1)
                                                   (my-run-freehand-notes-program-with-file ,searched-for-link-content))
                                                 "open (already exiting) annotating freehand notes"))
                                           ;; else: create annotating freehand notes
                                           '("c a f"
                                             (lambda ()
                                               (interactive)
                                               (auto-revert-mode 1)
                                               (my-create-new-freehand-note 'ann))
                                             "create (not yet existing) annotating freehand notes")))
                                      ("q" nil "cancel"))))))

    (hydra-klin-open-from-pdf-view/body)
    (fmakunbound 'hydra-klin-open-from-pdf-view/body)
    (setq hydra-klin-open-from-pdf-view nil)))

(define-key pdf-view-mode-map (kbd "C-M-, o") ; o: open
  'pdf-view-run-context-aware-hydra)

(define-key org-mode-map (kbd "C-M-, p") ; p: process
  (defhydra hydra-klin-process-from-org (:columns 3)
    "klin: process in org"
    ("i l o" (lambda ()
           (interactive)
           (klin-org-insert-latex-overhead)) "insert LaTeX overhead")))

;; ---------- presentations with org-mode, latex beamer and inkscape
;; (define-key org-mode-map (kbd "C-M-, i") 'klin-open-in-inkscape)

(define-key org-mode-map (kbd "C-M-, i")
  (defhydra hydra-insert-into-org ()
    "klin: insert into org"
    ("f h"
      (lambda ()
        (interactive)
        (my-create-new-freehand-note))
      "freehand note")))
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

;; (global-set-key (kbd "C-M-,") nil)
(define-key bibtex-mode-map (kbd "C-M-, p")
  (defhydra hydra-klin-from-bibtex (:columns 3)
    "klin in bibtex (edit)"
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
;; (global-set-key (kbd "C-. p") 'klin-pdf-helm-browse-pdf-buffers)
(global-set-key (kbd "C-. P") 'klin-tabs-open-pdfs)
(global-set-key (kbd "C-. b") 'klin-bibtex-set-bibfiles-for-pdfs)

;; (global-set-key (kbd "C-M-, e") nil)

(global-set-key (kbd "C-M-, m")          ; pop off/on
                (defhydra hydra-klin-pop-or-push
                  ()
                  "klin buffer pop/push"
                  ("o f"
                   (lambda ()
                     (interactive)
                     (hydra-disable)
                     (pop-buffer t t)
                     )
                   "off -> (new) frame")
                  ("o o"
                   (lambda ()
                     (interactive)
                     (pop-buffer t nil))
                   "off -> (just) off")
                  ("i i"
                   (lambda ()
                     (interactive)
                     (push-buffer))
                   "into -> window")
                  ("i s"
                   (lambda ()
                     (interactive)
                     (push-buffer t))
                   "into -> window")
                  ("i n t"
                   (lambda ()
                     (interactive)
                     (push-buffer nil t))
                   "into -> window")
                  ("c"
                   (lambda ()
                     (interactive)
                     (clear-popped-buffer-and-frame))
                   "into -> window")))

;; (global-set-key (kbd "C-. p o f")
;;                 (lambda () (pop-buffer 'pop-off 'new-frame)))
;; (global-set-key (kbd "C-. p i w")
;;                 (lambda () (push-buffer)))


;; (global-set-key (kbd "C-M-, k") 'kill-frame-and-buffers-within)

(provide 'klin-bindings)


;;; klin-bindings.el ends here
