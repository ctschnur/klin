;;; klin-keys.el --- key bindings for klin functions in different modes  -*- lexical-binding: t; -*-

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
;; minor mode (first option: buffer-local, second option: global),
;; or (2) they can be (easier option in my opinion) just set in hooks
;; when entering the specific file type or mode (e.g. org-mode) where
;; you want to have them available.  I'm going to do (2) here.

;;; Code:

(require 'frame-cmds)

;; (add-to-list 'load-path (expand-file-name (file-name-directory (buffer-file-name))))
(require 'klin-utils)
(require 'klin-org)
(require 'klin-bibtex)
(require 'klin-pdf-frames)

;; general
(global-set-key (kbd "C-, k") 'kill-frame-and-buffers-within)

;; more specific, but can be called from anywhere
;; maybe for some of them it would be good
;; to create a buffer-local minor mode
(global-set-key (kbd "C-, m") 'make-bibtex-file-for-pdf)
(global-set-key (kbd "C-, i") 'make-invisible)
(global-set-key (kbd "C-, v") 'make-visible)
(global-set-key (kbd "C-, p") 'helm-browse-pdf-buffers)

;; org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key 'org-mode-map (kbd "C-, o") 'search-nearest-link-and-open)))

(provide 'klin-keys)
;;; klin-keys.el ends here
