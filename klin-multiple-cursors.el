;;; klin-multiple-cursors.el --- integration with multiple-cursors  -*- lexical-binding: t; -*-

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

(define-key org-mode-map (kbd "C-M-, o")
  (lambda () ; multiple-cursors works ootb only with lambdas
    (interactive)
    (let* ((orig-window (selected-window))
           (orig-buffer (current-buffer))
           (orig-frame (selected-frame)))
      (klin-org-open-link-nearest-to-point)
      ;; multiple-cursors operates only reliably in the current frame
      ;; so you have to change back to the current frame after
      ;; opening up the n-m th frame in a series of n new frames
      (select-window orig-window))))

(provide 'klin-multiple-cursors)
;;; klin-multiple-cursors.el ends here
