;;; klin-pdfgrep.el --- helper functions for pdfgrep  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-thinkpad>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; --- override pdfgrep to include custom list of files
(require 'pdfgrep)

(setq pdfgrep-context-length 10)

(defun pdfgrep-goto-locus (_msg _mk _end-mk)
  "Jump to a match corresponding.
_MSG, _MK and _END-MK parameters are ignored.  This function is
used to advice `compilation-goto-locus'."
  (when (and (eq major-mode 'doc-view-mode)
	     (eq doc-view-doc-type 'pdf))
    (doc-view-goto-page (car (pdfgrep-current-page-and-match))))
  (when (eq major-mode 'pdf-view-mode)
    (let ((meta (pdfgrep-current-page-and-match)))
      (pdf-view-goto-page (car meta))
      (when (cdr meta)
        (let* ((edges (pdf-isearch-search-page (cdr meta)))
               (edges-of-current-match (nth (pdfgrep-current-page-match-number)
                                            edges)))

          ;; ;; focus only the current match
          ;; (pdf-isearch-hl-matches nil (list edges-of-current-match) t)
          ;; focus all matches
          (pdf-isearch-hl-matches nil edges t)
          ;; scroll the current match into view
          (pdf-util-scroll-to-edges (apply 'pdf-util-edges-union
                                           edges-of-current-match))
          ;; (put-tooltip-arrow edges-of-current-match)
          )
        ))))


(defun get-page-from-next-error-output (neo)
  (nth 1 (compilation--message->loc neo)))

(defun get-file-from-next-error-output (neo)
  (car (car (nth 2 (compilation--message->loc neo)))))

(defun pdfgrep-current-page-match-number ()
  "Return the current match page number and match string."
  (with-current-buffer pdfgrep-buffer-name
    (save-excursion
      (let* (;; (cur (buffer-substring (line-beginning-position)
             ;;                        (line-end-position)))
             ;; (start (text-property-any 0 (length cur) 'font-lock-face
             ;;                           'match cur))
             (pdf-page-prev (get-page-from-next-error-output (compilation-next-error 0)))
             (pdf-page-cur pdf-page-prev)
             (pdf-file-prev (get-file-from-next-error-output (compilation-next-error 0)))
             (pdf-file-cur pdf-file-prev)
             (ctr 0)
             comp-next-err)

        ;; from the current error go backwards until you hit
        ;; an error where the page isn't the same but the file still is
        ;; count the steps along the way -> number of match of the current pdf page
        (while (and (equal pdf-page-prev (setq pdf-page-cur (get-page-from-next-error-output (setq comp-next-err (compilation-next-error -1)))))
                    (string-equal pdf-file-prev
                                  (setq pdf-file-cur (get-file-from-next-error-output comp-next-err))))
          (setq ctr (+ 1 ctr))
          (setq pdf-file-prev pdf-file-cur)
          (setq pdf-page-prev pdf-page-cur))

        ctr
        ;; (substring cur start (next-property-change start cur))
        ))))

(defun pdfgrep-on-linked-files ()
  (interactive)
  (pdfgrep-mode 1)
  (let* ((sh-cmd (read-shell-command "Run pdfgrep (like this): "
                                 (cons (concat (pdfgrep-default-command)
                                               "\"\" "
                                               (string-join (mapcar (lambda (path)
                                                                      (prin1-to-string path))
                                                                    (klin-pdf-grep-get-candidates))
                                                            " "))
                                       (+ 2 (string-bytes (pdfgrep-default-command))))
                                 'pdfgrep-history))
         (new-frame (make-frame-same-dim-as-cur-window)))
    (select-frame new-frame)
    (pdfgrep sh-cmd)))

;; ---- helm source to pick out the custom list of files ----

(defun klin-pdf-grep-get-candidates ()
  "Within org buffer, search all referenced pdf filepaths and selectively run grep on them."
  ;; (interactive)
  (helm :sources `(((name . "PDFs to be grepped: ")
                    (candidates . ,(let* ((one-d-list (append (find-all-org-noter-pdf-documents-filepaths-in-buffer)
                                                              (find-all-org-mode-pdf-links-filepaths-in-buffer))))
                                     (mapcar (lambda (elem)
                                               (list elem elem))
                                             one-d-list)))
                    (action . (lambda (candidate)
                                (mapcar (lambda (elem)
                                          (car elem))
                                        (helm-marked-candidates))))))))


(defun find-all-org-mode-links-in-selected-region ()
  (let* (reg-beg reg-end
                 sel-text
                 (list-of-links '()))
    (when (region-active-p)
      (setq reg-beg (region-beginning))
      (setq reg-end (region-end))
      (setq sel-text (buffer-substring-no-properties reg-beg reg-end))
      (with-temp-buffer
        (insert " ")
        (insert sel-text)
        (org-mode)
        (goto-char (point-min))
        (setq list-of-links (find-all-org-mode-pdf-links-filepaths-in-buffer))))
    list-of-links))

(defun find-all-org-mode-pdf-links-filepaths-in-buffer ()
  (interactive)
  (save-excursion
    (let* ((org-linked-pdfs '())
           (previous-point (point-min))
           type
           path
           pdf-path)
      (goto-char (point-min))
      (while (progn
               (org-next-link)
               (not (eq previous-point (point))))
        (setq previous-point (point))
        (setq type (org-element-property :type (org-element-context)))
        (setq path (org-element-property :path (org-element-context)))
        (setq pdf-path (cond
                        ((string-equal type "pdfview")
                         (car (split-string path "::")))
                        ((string-equal type "file") path)
                        ((string-equal type "cite")
                         (my-get-org-link-at-point-cited-reference-pdf-file-path))))
        (if (stringp pdf-path)
            (if (and (string-equal (file-name-extension pdf-path)
                                   "pdf")
                     (file-exists-p pdf-path))
                (setq org-linked-pdfs (append org-linked-pdfs (list pdf-path))))))
      (mapcar (lambda (path)
                (expand-file-name (string-trim path)))
              (delq nil
                    (delete-dups org-linked-pdfs))))))

(defun find-all-org-noter-pdf-documents-filepaths-in-buffer ()
  (interactive)
  (save-excursion
    (let* ((org-noter-linked-pdfs '())
           (previous-point (point-min))
           type
           path
           pdf-path)
      (goto-char (point-min))
      (while (progn
               (re-search-forward ":NOTER_DOCUMENT:" nil
                                  'continue)
               (not (eq previous-point (point))))
        (setq previous-point (point))
        (setq pdf-path (expand-file-name (string-trim (buffer-substring-no-properties (match-end 0)
                                                                                      (progn
                                                                                        (end-of-line)
                                                                                        (point))))))
        (if (stringp pdf-path)
            (if (and (string-equal (file-name-extension pdf-path)
                                   "pdf")
                     (file-exists-p pdf-path))
                (setq org-noter-linked-pdfs (append org-noter-linked-pdfs (list pdf-path))))))
      (delq nil
            (delete-dups org-noter-linked-pdfs))))
  )

(provide 'klin-pdfgrep)
;;; klin-pdfgrep.el ends here
