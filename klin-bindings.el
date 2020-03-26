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

(require 'klin-pdf-toggle)
(require 'klin-image-toggle)

;; ------- if not (yet) in org-noter -------
(require 'klin-org-noter)
(define-key org-mode-map (kbd "C-S-M-l") 'org-noter)
(define-key org-mode-map (kbd "C-S-M-p") 'org-noter-jump-into-it-prev-note)
(define-key org-mode-map (kbd "C-S-M-n") 'org-noter-jump-into-it-next-note)
;; ----------

;; -------- within org-noter ---------
(with-eval-after-load 'org-noter
  (define-key org-noter-notes-mode-map (kbd "C-S-M-p") 'org-noter-sync-prev-note-in-prev-pdf)
  (define-key org-noter-notes-mode-map (kbd "C-S-M-n") 'org-noter-sync-next-note-in-next-pdf)
  (define-key org-noter-notes-mode-map (kbd "C-S-M-h") 'org-noter-switch-to-base-buffer))

(define-key org-noter-notes-mode-map (kbd "C-M-, k k") 'org-noter-kill-session)
(define-key org-noter-doc-mode-map (kbd "C-M-, k k") 'org-noter-kill-session)

;; -------- org-mode important klin keys
(define-key org-mode-map (kbd "C-M-, l") 'klin-org-insert-pdf-link)
(define-key org-mode-map (kbd "C-M-, b j") 'org-ref-open-citation-at-point)
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


;; ------ search from org --------

(defun org-run-search-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-search-from-org
                                      (:columns 3 :exit t)
                                      "klin: search/grep from org"
                                      ("p g l"
                                       (lambda ()
                                         (interactive)
                                         (pdfgrep-on-linked-files))
                                       "pdfgrep on linked pdf files")
                                      ("q" nil "cancel"))))))
    (hydra-klin-search-from-org/body)
    (fmakunbound 'hydra-klin-search-from-org/body)
    (setq hydra-klin-search-from-org/body nil)))

(define-key org-mode-map (kbd "C-M-, s") 'org-run-search-hydra)


;; --------- open from org ---------

(defun klin-org-freehand-hydra ()
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
    (hydra-klin-open-from-org/body)
    (fmakunbound 'hydra-klin-open-from-org/body)
    (setq hydra-klin-open-from-org/body nil)))

(define-key org-mode-map (kbd "C-M-, f") 'klin-org-freehand-hydra)


;; ----------

(defun org-run-watch-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-watch-from-org
                                      (:columns 3 ;; :exit t
                                                )
                                      "klin: watch from org"
                                      ("s"
                                       (lambda ()
                                         (interactive)
                                         (klin-org-watch-and-insert-scanned-file))
                                       "watch single")
                                      ("c"
                                       (lambda ()
                                         (interactive)
                                         (watch-and-insert-arriving-files))
                                       "watch cont.")
                                      ("m t f"
                                       (lambda ()
                                         (interactive)
                                         ;; (message "hi")
                                         (get-two-files-and-ask-merge))
                                       "merge")
                                      ("m m s"
                                       (lambda ()
                                         (interactive)
                                         ;; (message "hi")
                                         (pdf-merge-two-pdfs-from-scan-from-my-printer))
                                       "merge (from my scanner)")
                                      ("q"
                                       (lambda ()
                                         (interactive)
                                         (quit-watch))
                                       "quit watch")
                                      )))))
    (hydra-klin-watch-from-org/body)
    (fmakunbound 'hydra-klin-watch-from-org/body)
    (setq hydra-klin-watch-from-org/body nil)))

(define-key org-mode-map (kbd "C-M-, w")
  'org-run-watch-hydra)

(defun klin-run-pdfview-freehand-hydra ()
  (interactive)
  (let* ((link-content-from-pdf-view (file-name-base (buffer-file-name)))
         (annotated-pdf-filepath (concat (my-get-freehand-note-filepath-associated-pdf-filepath (my-get-freehand-note-annotating-filename
                                                                                                 link-content-from-pdf-view))))
         (created-annotated-pdf-path (concat (file-name-directory (buffer-file-name))
                                             (my-get-freehand-note-annotating-filename-base (file-name-base (buffer-file-name)))
                                             ".pdf"))
         (searched-for-link-content (concat link-content-from-pdf-view my-annotating-freehand-notes-filename-tag)))
    (let* ((hydra-body (eval (remove nil
                                     `(defhydra hydra-klin-open-from-pdf-view
                                        (:columns 2 :exit t)
                                        "klin: open from pdf-view"
                                        ;; if generating freehand notes exist

                                        ,(if (file-exists-p (concat (file-name-sans-extension (buffer-file-name))
                                                                    "."
                                                                    my-freehand-notes-extension))
                                             (let* ()
                                               `("f g"
                                                 (lambda ()
                                                   (interactive)
                                                   (auto-revert-mode 1)
                                                   (my-run-freehand-notes-program-with-file ,link-content-from-pdf-view))
                                                 "open (existing) generating freehand notes")
                                               (let* ()
                                                 (when (my-is-filepath-annotating-p (buffer-file-name))
                                                   `("s"
                                                     (lambda ()
                                                       (interactive)
                                                       (auto-revert-mode 1)
                                                       (find-file-existing ,(concat (my-get-filepath-base-generating (buffer-file-name))
                                                                                    ".pdf")))
                                                     "switch to (generating) pdf")))))
                                        ;; if annotating freehand notes exist

                                        ,(let* (try-file open-file searched-for-link-content)
                                           (cond
                                            ( ;; it's a generating pdf that is shown and you want to find the existing annotating file
                                             (file-exists-p (setq try-file (concat (my-get-freehand-note-annotating-filename
                                                                                    link-content-from-pdf-view))))
                                             (setq open-file (expand-file-name try-file))
                                             (setq searched-for-link-content (file-name-base try-file)))
                                            ( ;; it's an annotated pdf that is shown and you wish to find the corresponding annotating file
                                             (file-exists-p (setq try-file (concat (my-get-freehand-note-filepath-from-annotated-pdf-filepath (buffer-file-name)))))
                                             (setq open-file (expand-file-name try-file))
                                             (setq searched-for-link-content (file-name-base try-file))
                                             ))

                                           (when open-file
                                             `("f a"
                                               (lambda ()
                                                 (interactive)
                                                 (auto-revert-mode 1)
                                                 (my-run-freehand-notes-program-with-file ,searched-for-link-content)
                                                 (find-file ,(my-get-freehand-note-filepath-associated-pdf-filepath try-file))
                                                 (pdf-view-redisplay)
                                                 (auto-revert-mode 1))
                                               "open (already exiting) annotating freehand notes")))
                                        ,(when (not (my-is-filepath-annotating-p (buffer-file-name)))
                                           (let* ()
                                             `("c a f"
                                               (lambda ()
                                                 (interactive)
                                                 (my-create-new-freehand-note 'ann)
                                                 (find-file ,created-annotated-pdf-path)
                                                 (pdf-view-redisplay)
                                                 (auto-revert-mode 1))
                                               "create (not yet existing) annotating freehand notes")))
                                        ,(when (file-exists-p annotated-pdf-filepath)
                                           `("s"
                                             (lambda ()
                                               (interactive)
                                               (auto-revert-mode 1)
                                               (find-file-existing ,annotated-pdf-filepath))
                                             "switch to (annotated) pdf"))
                                        ("i b"
                                          (lambda ()
                                            (interactive)
                                            (klin-open-pdf-in-browser))
                                          "open in browser")
                                        ("q" nil "cancel"))))))

      (hydra-klin-open-from-pdf-view/body)
      (fmakunbound 'hydra-klin-open-from-pdf-view/body)
      (setq hydra-klin-open-from-pdf-view nil))))

(define-key pdf-view-mode-map (kbd "C-M-, f") 'klin-run-pdfview-freehand-hydra)


;; ----- process pdf file  ------------

(defun pdf-view-run-context-aware-hydra-process ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-process-from-pdf-view
                                      (:columns 3 :exit t)
                                      "klin: process from pdf-view"
                                      ("r c w"
                                             (lambda ()
                                               (interactive)
                                               (auto-revert-mode 1)
                                               (pdf-view-rotate-clockwise))
                                             "rotate clockwise (overrides)")
                                        ("r c c w"
                                             (lambda ()
                                               (interactive)
                                               (auto-revert-mode 1)
                                               (pdf-view-rotate-counterclockwise))
                                             "rotate counter-clockwise (overrides)")

                                      ("q" nil "cancel"))))))
  (hydra-klin-process-from-pdf-view/body)
  (fmakunbound 'hydra-klin-process-from-pdf-view/body)
  (setq hydra-klin-process-from-pdf-view/body nil)))

(define-key pdf-view-mode-map (kbd "C-M-, p") ; process
  'pdf-view-run-context-aware-hydra-process)

;; ----------

(define-key org-mode-map (kbd "C-M-, p") ; p: process
  (defhydra hydra-klin-process-from-org (:columns 1)
    "klin: process in org"
    ("i l o" (lambda ()
           (interactive)
           (klin-org-insert-latex-overhead)) "insert LaTeX overhead")
    ("i l r" (lambda ()
           (interactive)
           (klin-org-insert-latex-references-section)) "insert LaTeX references section")))

;; ---------- presentations with org-mode, latex beamer and inkscape
;; (define-key org-mode-map (kbd "C-M-, i") 'klin-open-in-inkscape)


(defun klin-run-hydra-latex-tools ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra klin-hydra-latex-tools
                                      (:columns 3 ;; :exit t
                                                )
                                      "klin: watch from org"
                                      ("o i i"
                                       (lambda ()
                                         (interactive)
                                         (klin-open-in-inkscape))
                                       "open in Inkscape")
                                      ("q"
                                       (lambda ()
                                         (interactive)
                                         (quit-watch))
                                       "cancel")
                                      )))))
    (klin-hydra-latex-tools/body)
    (fmakunbound 'klin-hydra-latex-tools/body)
    (setq klin-hydra-latex-tools/body nil)))


;; (require 'latex)
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-M-, t")
    'klin-run-hydra-latex-tools))
;; (load "latex.el")

;; ---------

(define-key org-mode-map (kbd "C-M-, i")
  (defhydra hydra-insert-into-org (:exit t)
    "klin: insert into org"
    ("f h"
      (lambda ()
        (interactive)
        (my-create-new-freehand-note))
      "freehand note")
    ("o n h p s"
     (lambda ()
       (interactive)
       (org-noter-insert-pdf-headings))
     "org-noter headings from pdfs select")
    ("o n h p l"
     (lambda ()
       (interactive)
       (org-noter-insert-pdf-headings (find-all-org-mode-links-in-selected-region)))
     "org-noter headings from pdf links")
    ))

;; -------

;; ------- within a bibtex buffer
(define-key bibtex-mode-map (kbd "C-M-, e o p") 'klin-bibtex-entry-open-pdf)
(define-key bibtex-mode-map (kbd "C-M-, g t i") 'klin-bibtex-grab-template-isbn)
(define-key bibtex-mode-map (kbd "C-M-, e f f") 'klin-bibtex-entry-fix-filepath-file-page-offset)   ; fix entry file-
(define-key bibtex-mode-map (kbd "C-M-, e i c") 'klin-bibtex-integrate-bib-entry-into-collective)
(define-key bibtex-mode-map (kbd "C-M-, o p") 'klin-bibtex-open-pdf-from-bibtex)

;; (global-set-key (kbd "C-M-,") nil)
(define-key bibtex-mode-map (kbd "C-M-, p")
  (defhydra hydra-klin-from-bibtex (:columns 1)
    "klin in bibtex (edit)"
    ("o p b"
      (lambda ()
        (interactive)
        (klin-bibtex-open-pdf-from-bibtex))
      "open pdf")
    ("g t i" (lambda ()
           (interactive)
           (klin-bibtex-grab-template-isbn)) "grab template from ISBN")
    ("g t d" (lambda ()
           (interactive)
           (call-interactively 'get-bibtex-from-doi)) "grab template from DOI")
    ("f p" (lambda ()
             (interactive)
             (klin-bibtex-entry-fix-filepath-field)) "fix file-path")
    ("f o" (lambda ()
             (interactive)
             (klin-bibtex-entry-fix-file-page-offset)) "fix file-page-offset")
    ("e i c" (lambda ()
               (interactive)
               (klin-bibtex-integrate-bib-entry-into-collective)) "integrate into collective")
    ("c o" (lambda ()
           (interactive)
           (klin-bibtex-compare-entry-to-original-bibfile))
     "comp. coll[..].bib, self.bib")
    ("F" (lambda ()
           (interactive)
           (klin-bibtex-process-finish-current-pdf)) "finish current")
    ("c l" (lambda ()
           (interactive)
           (klin-bibtex-process-clean-whole-list)) "clean out list")
    ("n" (lambda ()
           (interactive)
           (klin-bibtex-process-next-pdf)) "next")
    ("p" (lambda ()
           (interactive)
           (klin-bibtex-process-previous-pdf)) "previous")
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
(global-set-key (kbd "C-. b") 'klin-bibtex-set-bibfiles-for-pdfs)

;; (global-set-key (kbd "C-M-, e") nil)

(global-set-key (kbd "C-M-, m")          ; pop off/on
                (defhydra hydra-klin-pop-or-push
                  (:columns 1)
                  "klin buffer pop/push"
                  ("s b n f"
                   (lambda ()
                     (interactive)
                     (klin-open-same-buffer-in-new-frame))
                   "open same buffer in new frame")
                  ("o f"
                   (lambda ()
                     (interactive)
                     (hydra-disable)
                     (pop-buffer t t))
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
                   "into new elcreen tab")
                  ("k l f"
                   (lambda ()
                     (interactive)
                     (klin-clone-into-new-frame))
                   "clone (klone) into new frame")
                  ("k 2"
                   (lambda ()
                     (interactive)
                     (klin-clone-into-new-frame))
                   "clone (klone) into new frame")
                  ("c"
                   (lambda ()
                     (interactive)
                     (clear-popped-buffer-and-frame))
                   "clear")
                  ("q"
                   nil
                   "quit")))

;; (global-set-key (kbd "C-M-, k") 'kill-frame-and-buffers-within)


(defun klin-run-library-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-library
                                      (:columns 1 :exit t)
                                      "klin: search/grep from org"
                                      ("g n"
                                       (lambda ()
                                         (interactive)
                                         (grep-find-in-notes-directories nil "academics"))
                                       "grep notes")
                                      ("c p"
                                       (lambda ()
                                         (interactive)
                                         (call-interactively 'klin-create-physics-note))
                                       "create new physics note")
                                      ("c n"
                                       (lambda ()
                                         (interactive)
                                         (call-interactively 'klin-create-physics-note))
                                       "create new physics note")
                                      ("t o p"
                                       (lambda ()
                                         (interactive)
                                         (klin-tabs-open-pdfs))
                                       "tabs open pdfs")
                                      ("q" nil "cancel"))))))
    (hydra-klin-library/body)
    (fmakunbound 'hydra-klin-library/body)
    (setq hydra-klin-library/body nil)))

(global-set-key (kbd "C-M-, L") 'klin-run-library-hydra)


(defun klin-run-project-launcher-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-project-launcher
                                      (:columns 1 :exit t)
                                      "klin: project launcher"
                                      ("c p"
                                       (lambda ()
                                         (interactive)
                                         (call-interactively 'klin-create-physics-note))
                                       "create new physics note")
                                      ("c n"
                                       (lambda ()
                                         (interactive)
                                         (call-interactively 'klin-populate-project-file-structure))
                                       "populate project file structure")
                                      ("q" nil "cancel"))))))
    (hydra-klin-project-launcher/body)
    (fmakunbound 'hydra-klin-project-launcher/body)
    (setq hydra-klin-project-launcher/body nil)))

(global-set-key (kbd "C-M-, C") 'klin-run-project-launcher-hydra)


(defun klin-run-link-processor-hydra (&optional link)
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-klin-link-processor
                                      (:columns 1)
                                      ,(concat "klin: process link" (prin1-to-string link))
                                      ("f r"
                                       (lambda ()
                                         (interactive)
                                         (message "finding and replacing...")
                                         )
                                       "find cite")
                                      ("q" nil "cancel"))))))
    (hydra-klin-link-processor/body)
    (fmakunbound 'hydra-klin-link-processor/body)
    (setq hydra-klin-link-processor/body nil)))

;; (global-set-key (kbd "C-M-, p") 'klin-run-link-processor-hydra)

(global-set-key (kbd "C-x C-p") 'counsel-fzf)

(provide 'klin-bindings)

;;; klin-bindings.el ends here
