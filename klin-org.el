;;; klin-org.el --- Klin functions to call from org document  -*- lexical-binding: t; -*-

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

;; These functions should include:
;; - function to insert the latest bookmark created from a pdf page as a dynamic link
;; - function to highlight and open the next dynamic link to a pdf page
;; (optional:
;;  - function to create custom org link as a dynamic link)

;;; Code:

  (interactive)
  ;; package org-pdfview with org-pdfview.el
  ;; override a function there
  (eval-after-load "org-pdfview"
    (defun org-pdfview-store-link ()
      (when (eq major-mode 'pdf-view-mode)
        (let* ((type "cite")
               (filename (file-name-nondirectory (buffer-file-name)))
               ;; make sure there is a proper bibtex file associated with it, otherwise create it
               ;; (bibtexkey )
       	       (link (concat type ":" bibtexkey))
               ;; todo: put a hidden .bib file for every book in your pdf directory, with page-offset
               (page-offset 0)
               (page-in-pdf (pdf-view-current-page))
               (page-in-print (- page-in-pdf page-offset))
               (description (concat "p." (number-to-string page-in-print)))
              )
          (org-store-link-props
           :type type
           :link link
           :description description))))))

(defun openlink (mylist)
  (setq linktyp (nth 0 mylist))
  (setq bibtexkey (nth 1 mylist))
  (setq description (nth 2 mylist))
  (string-match "p\\.\\s-*\\([0-9]*\\)" description)
  (setq page-str (match-string 1 description))
  (klin-open-pdf-from-bibtex bibtexkey (string-to-number page-str))
  )

(defun search-nearest-link-and-open ()
  (interactive)
  (openlink (get-link-info-nearest-to-point))
  )

(defun get-link-text-at-point ()
  (setq type (org-element-context))
  (if (eq (car (org-element-context)) 'link)
      (progn
        (setq buf-substr (buffer-substring-no-properties
                          (org-element-property :begin type) (org-element-property :end type))))))

(defun get-link-info-nearest-to-point ()
  "This could be done easier: if it's not on a link, go to previous/next link then extract
   or if it's on a link, do nothing but extract"
  ;; check if at point there is a link
  (setq re "\\[\\[\\(.*?\\):\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
  (setq nearestlink-string (get-link-text-at-point))
  (if (not nearestlink-string)
    (progn
      (setq poscur (point))
      (save-excursion
        (re-search-forward re nil t 1)
        (setq nextlink-match-beginning (match-beginning 0))
        (setq nextlink-match-end (match-end 0))
        (setq nextlink-match-string (match-string-no-properties 0)))
      (save-excursion
        (re-search-backward re nil t 1)
        (setq prevlink-match-beginning (match-beginning 0))
        (setq prevlink-match-end (match-end 0))
        (setq prevlink-match-string (match-string-no-properties 0)))

       ;; find the nearest one
       (if (< (abs (- prevlink-match-end poscur)) (abs (- nextlink-match-beginning poscur)))
           (progn
             (setq nearestlink-beginning prevlink-match-beginning)
             (setq nearestlink-string prevlink-match-string))
         (progn
           (setq nearestlink-beginning nextlink-match-beginning)
           (setq nearestlink-string nextlink-match-string)))))

   ;; apply the regex again to the nearest string and copy the data
   (string-match re nearestlink-string)
   (list (match-string 1 nearestlink-string)
         (match-string 2 nearestlink-string)
         (match-string 3 nearestlink-string)))


(defun org-ref-find-bibliography-fullfilenames (&optional org-buffer)
  "to be applied inside an org buffer"
  (interactive)
  (if (not org-buffer)
      (progn
        ;; if the current buffer from wich this function is called is an org buffer, then use that one
        (if (string= "org" (file-name-extension (buffer-name)))
            (setq org-buffer (current-buffer))
          (message this-command "not called from an org file and option org-buffer not provided. "
                   "No clue given what org-buffer to use."))
        ))

  ;; (unless org-buffer (setq org-buffer (get-buffer "main.org"))) ;; debugging

  ;; find partial filenames relative to org buffer in which e.g. addbibresources are defined
  (with-current-buffer org-buffer
    (let ((i 0)
          (list-of-property-strings (org-ref-find-bibliography))
          list-of-full-referenced-bibtex-filepaths)
      (while (< i (length list-of-property-strings))
        (let* ((bibresource-file-name (substring-no-properties (nth i list-of-property-strings)))
               (bibresource-base-dir (file-name-directory (buffer-file-name)))
               (full-referenced-bibtex-filepath (concat bibresource-base-dir bibresource-file-name)))
          (setq list-of-full-referenced-bibtex-filepaths
                (append list-of-full-referenced-bibtex-filepaths `(,full-referenced-bibtex-filepath)))
          )
        (setq i (+ i 1))
        )
      list-of-full-referenced-bibtex-filepaths
      )
    )
  )

(defun make-invisible ()
  "Make the current frame invisible."
  (interactive)
  (make-frame-invisible (window-frame (get-buffer-window (current-buffer) t))))

(defun make-visible (&optional bufname)
  "Make frame with a certain BUFNAME in it visible."
  (interactive)
  (unless bufname
    (setq bufname "elberfelder-1905-deuelo_a4.pdf"))
  (let* ((buffer (get-buffer bufname))
         (bufwindow (get-buffer-window buffer t)))
    (if bufwindow
        (make-frame-visible (window-frame bufwindow))
      ;; (setq newframe (make-frame))
      ;; (select-frame newframe)
      ;; (when (display-graphic-p frame)
      ;; (switch-to-buffer buffer)
      (switch-to-buffer-other-frame bufname)
      ;; (message (concat "current buffer: " (buffer-name (current-buffer))))
      (pdf-view-redisplay) ;; That fixed the raw-pdf "fundamentalmode" stalling for me in emacs 25.2.2 and pdf-tools 1.0
      ;; (message (concat "i tried pdf-view-redisplay"))
      )))

(provide 'klin-org)
;;; klin-org.el ends here
