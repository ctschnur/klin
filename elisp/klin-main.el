(require 'frame-cmds)

(add-to-list 'load-path (expand-file-name (file-name-directory (buffer-file-name))))
(load "klin-utils")
(load "klin-org")
(load "klin-bibtex")

(global-set-key (kbd "C-, k") 'kill-frame-and-buffers-within)
(global-set-key (kbd "C-, m") 'make-bibtex-file-for-pdf)
(global-set-key (kbd "C-, i") 'make-invisible)
(global-set-key (kbd "C-, o") 'search-nearest-link-and-open)
(global-set-key (kbd "C-, p") 'helm-browse-pdf-buffers)
(global-set-key (kbd "C-, i") 'make-invisible)
(global-set-key (kbd "C-, v") 'make-visible)
(global-set-key (kbd "C-2") 'helm-mini)  ;; select buffers with C-Space, delete selection with M-S-d
