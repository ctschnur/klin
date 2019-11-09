;;; klin-presentations.el --- tools for dealing with org-mode, latex beamer and inkscape  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris(defun get-ffap-file-at-point-inside-curly-brackets (&optional just-contents) <chris@chris-IdeaPad-U330p>
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

(require 'ox-latex)
(require 'ob-tangle)
(require 'org)

;; ----- setup so that org-mode and latex can work together ----
;; (this could also just be shipped with every org-mode file
;; and then tangled by org-babel, if you so want)

(defun klin-org-mode-latex-beamer-bare-minimum-setup ()
  "Do initial housekeeping to get `org-mode' working with latex beamer.
Of course, this function is to be configured as the workflow needs to
be adapted."
  (interactive)
  (setq org-latex-pdf-process '("latexmk -pdf -pdflatex='lualatex --shell-escape' -bibtex %f"))
  ;; (setq org-latex-pdf-process '("latexmk -pdf -pdflatex=lualatex -bibtex %f"))
  (setq org-latex-listings 'minted)
  (setq org-latex-packages-alist '(("" "minted")))
  (setq org-latex-minted-options '(("bgcolor" "bg")
                                   ("frame" "lines")
                                   ("fontsize" "\\scriptsize")))

  (setq org-babel-python-command "python3")

  ;; (setq org-latex-listing t)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (eval-after-load "ox-latex"
    ;; update the list of LaTeX classes and associated header (encoding, etc.)
    ;; and structure

    '(add-to-list 'org-latex-classes
                  `("beamer" ,(concat "\\documentclass[presentation]{beamer}\n"
                                      "[DEFAULT-PACKAGES]" "[PACKAGES]" "[EXTRA]\n")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))


;; ----- easy interaction with inkscape -----

(defun get-ffap-file-at-point-inside-curly-brackets (&optional just-contents)
  "If JUST-CONTENTS is set to non-nil, give back just the contents (as-is)
unaltered by ffap-file-at-point, which actually checks if there is a file."
  (interactive)
  (let* (bracket-pos-backward
         bracket-pos-forward
         string-inside
         )
    (save-excursion
      (setq bracket-pos-backward (re-search-backward "{"))
      (setq bracket-pos-forward (re-search-forward "}")))
    (setq string-inside (buffer-substring-no-properties (+ bracket-pos-backward 1)
                                                        (- bracket-pos-forward 1)))
    (if just-contents
        string-inside
      (with-temp-buffer
        (insert string-inside)
        (ffap-file-at-point)))))

(defun klin-open-in-inkscape (&optional filepath)
  "Open a filepath (FILEPATH) within a latex command embedding a file, and it opens inkscape."
  (interactive)
  (let* ((base-dir (file-name-directory (buffer-file-name)))
         atpoint)
    (interactive)
    (unless filepath
      (setq filepath (setq atpoint (let* ((just-ffap (ffap-file-at-point))
                                          ;; sometimes, if there is more than 1 file
                                          ;; that fits, it just gives back the directory
                                          (is-directory (when just-ffap (string-equal just-ffap (file-name-as-directory just-ffap)))))
                                     (let* ((possibly-the-rel-filepath
                                             (concat (get-ffap-file-at-point-inside-curly-brackets
                                                      'just-contents)
                                                     ".svg"))
                                            (possibly-the-abs-filepath
                                             (concat base-dir possibly-the-rel-filepath)))
                                       (if is-directory
                                           (if (file-exists-p possibly-the-abs-filepath)
                                               possibly-the-abs-filepath)
                                         (setq possibly-the-rel-filepath (get-ffap-file-at-point-inside-curly-brackets
                                                                          'just-contents)))
                                       (helm-read-file-name "which file do you mean?: "
                                                                  :initial-input possibly-the-rel-filepath)))))
      ;; if the file doesn't exist, ask to create the file
      (if (or (not atpoint) (not filepath))
          (progn
            (setq filepath (helm-read-file-name "select svg file to open (and if necessary create directories): "
                                                :initial-input base-dir
                                                ))
            (make-directory (file-name-directory filepath)
                            t))
        )
      )

    (unless atpoint
      ;; insert relative filepath
      (insert (file-relative-name filepath base-dir)))

    (let* (inkscape-pid-file
           inkscape-pid)
      (call-process-shell-command (concat (unless (file-exists-p filepath)
                                            (concat "cp ~/.config/inkscape/templates/drawing.svg "
                                                    filepath
                                                    " && "))
                                          "inkscape "
                                          filepath
                                          ;; " && inkscape --export-pdf=" (file-name-sans-extension (buffer-file-name))
                                          ;; ".pdf"
                                          " & "
                                          "echo $! >"
                                          (setq inkscape-pid-file
                                                (concat "/tmp/" (make-temp-name "inkscape-svg-pid-for-bg-export"))))
                                  ;; nil
                                  ;; "*Shell Command Output*"
                                  ;; t
                                  ))))

;; --------


(provide 'klin-presentations)
;;; klin-presentations.el ends here
