
;;; Code:

;; (require 'frame-cmds)

(add-to-list 'load-path (expand-file-name (file-name-directory (buffer-file-name))))
(require 'klin-utils)
(require 'klin-org)
(require 'klin-bibtex)

;; autoload defers loading of the mode code until the mode is enabled
;; ;;;###autoload
;; (define-minor-mode klin-mode
;;   "my academic writing workflow fuctions, supposed to be working together
;;    and interacting across multiple buffers"
;;   :lighter " klin"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c f") 'insert-foo)
;;             (define-key map (kbd "C-, k") 'kill-frame-and-buffers-within)
;;             (define-key map (kbd "C-, m") 'make-bibtex-file-for-pdf)
;;             (define-key map (kbd "C-, i") 'make-invisible)
;;             (define-key map (kbd "C-, o") 'search-nearest-link-and-open)
;;             (define-key map (kbd "C-, p") 'helm-browse-pdf-buffers)
;;             (define-key map (kbd "C-, i") 'make-invisible)
;;             (define-key map (kbd "C-, v") 'make-visible)
;;             ;; select buffers with C-Space, delete selection with M-S-d
;;             (define-key map (kbd "C-2") 'helm-mini)
;;             map)
;;   :global t
;;   )

(provide 'klin-main)
;;; klin-main ends here
