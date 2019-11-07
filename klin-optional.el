;;; klin-optional.el --- klin-optional.el                      -*- lexical-binding: t; -*-

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

;; Here, I put some functions that can be useful to the workflow
;; but are not cure functionality of klin
;; (e.g. interfacing with org-noter, according to my personal convention)

;;; Code:

;; if a pdf in my library is called example.pdf, then
;; it should have an associated notes file .example.pdf.org


(require 'org-noter)

(require 'klin-utils)

(require 'cl-lib)

;; ---------- org-mode getting and setting global document properties
;; thanks to Tobias' answer at https://emacs.stackexchange.com/a/21472

;; -------- file add watcher in scanner
(require 'filenotify)

(defvar desc-global nil
  "Descriptor for the callback.")

(defun klin-watch-callback (event)
  "Insert org link (filepath inside EVENT) to file changed in cloud."
  (interactive)
  (let* ((filepath (expand-file-name (nth 2 event))))
    ;; removing the watcher immediateley
    ;; (message "Event %S" event)
    ;; (message "descriptor: ")
    ;; (print (car (car event)))
    ;; (message "action: ")
    ;; (print (nth 1 event))
    (if (eq (nth 1 event) 'created)
        (progn
          ;; (message "CREATED recognized!")
          ;; (insert "a")
          (message "%s was added" filepath)
          (if (eq major-mode 'org-mode)
              (insert (concat "[["
                              (klin-utils-get-reduced-file-path filepath)
                              "]"
                              "[scan:"
                              (file-name-nondirectory filepath)
                              "]]"))
            (message "not in org-mode, not inserting!"))))
    (if desc-global
        (file-notify-rm-watch desc-global))
    ;; (run-at-time "1 sec"
    ;;              nil
    ;;              (lambda ()
    ;;                (file-notify-rm-watch desc-global)
    ;;                (message "Removed the filesystem watcher.")))
    ))

(defun klin-org-watch-and-insert-scanned-file ()
  "Launch file system watcher."
  (interactive)
  (let* ((cloud-scanner-folder
          (expand-file-name "~/Dropbox/1LinkedApps/scanner/"))
         desc)
    (if (eq major-mode 'org-mode)
        (progn
          (setq desc (file-notify-add-watch cloud-scanner-folder
                                            '(change attribute-change)
                                            'klin-watch-callback))
          (setq desc-global desc)
          (message "watching for changes in %s, descriptor: %S"
                   cloud-scanner-folder
                   desc))
      (message "Not in org-mode, not setting up a watcher!."))))

;; (add-to-list 'org-capture-templates
;;              '("x" "Template Name" plain
;;               (function my/org-file-by-date)
;;               "Capture template contents"))


(defun visit-freehand-link (link-content)
  "This is what happens if the link of type xopp is clicked."
  (let* ((xopp-filepath
          (concat (file-name-directory (buffer-file-name)) link-content ".xopp")))
    (if (file-exists-p xopp-filepath)
        (command-to-launch-xopp (concat "xournalpp " xopp-filepath " & "))
      ;; else, just create it.
      )))


(org-add-link-type "freehand" #'visit-freehand-link)

(defun my-get-new-freehand-note-filename ()
  "Get filename base based off timestamp."
  (let* ((extension "xopp")
         (timestamp (format-time-string "%Y-%m-%d--%H-%M-%S")))
    (concat timestamp "." extension)))

(defun my-get-freehand-note-fliepath-associated-pdf-filepath (freehand-note-filepath)
  "Get the assoc pdf file path."
  (concat (file-name-directory freehand-note-filepath) (file-name-base freehand-note-filepath) ".pdf"))

(defun my-get-freehand-note-template-file-path ()
  "Get xopp template file path."
  (let* ((template-path (expand-file-name "~/.xournalpp/templates/plain-a4-template.xopp")))
    (when (file-exists-p template-path)
      template-path)))

(defun my-command-to-launch-freehand-program (&optional what-file-path)
  (interactive)
  (when what-file-path
    (concat "xournalpp " what-file-path " & ")))

(defun my-command-to-launch-background-auto-pdf-exporter-for-freehand-note (freehand-note-filepath)
  "Entr, export automatically to PDF.
FIXME: automatically terminate the entr session after closing xournal."
  (concat " echo " freehand-note-filepath " | " " entr "
          " xournalpp " freehand-note-filepath " -p "
          (my-get-freehand-note-fliepath-associated-pdf-filepath
           freehand-note-filepath)
          " & "))

;; TODO: continue with this
;; (defun klin-org-noter-make-xournalpp-note-and-insert-link ()
;;   (interactive)
;;   (let* ((new-freehand-note-filename (my-get-new-freehand-note-filename)))
;;     ;; (copy-file )
;;     (if (file-exists-p xopp-file-path)
;;         (progn
;;           ;; launch the program
;;           ;; (shell-command combined-launch-command-entr-xopp)
;;           (shell-command command-to-launch-xopp)
;;           ;; insert a link with a command to open up the xopp file
;;           (insert (concat
;;                    "( "
;;                    (concat "[[xopp:" combined-launch-command-entr-xopp "][" xopp-file-name-base "]]")
;;                    ", "
;;                    conventional-pdf-file-path
;;                    " )"))))))

;; --------

(defun org-global-props-key-re (key)
  "Construct a regular expression matching key and an optional plus and eating the spaces behind.
Test for existence of the plus: (match-beginning 1)"
  (concat "^" (regexp-quote key) "\\(\\+\\)?[[:space:]]+"))

(defun org-global-props (property &optional buffer)
  "Get the plists of global org properties of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-equal (org-element-property :key el) property) (nth 1 el))))))

(defun org-global-prop-value (property key)
  "Get global org property KEY of current buffer.
Adding up values for one key is supported."
  (let ((key-re (org-global-props-key-re key))
    (props (org-global-props))
    ret)
    (cl-loop with val for prop in props
         when (string-match key-re (setq val (plist-get prop :value))) do
         (setq
          val (substring val (match-end 0))
          ret (if (match-beginning 1)
              (concat ret " " val)
            val)))
    ret))

(defun org-global-prop-set (property key value)
  "Set the value of the first occurence of
#+PROPERTY: KEY
add it at the beginning of file if there is none."
  (save-excursion
    (let* ((key-re (org-global-props-key-re key))
       (prop (cl-find-if (lambda (prop)
                   (string-match key-re (plist-get prop :value)))
                 (org-global-props))))
      (if prop
      (progn
        (assert (null (match-beginning 1)) "First occurence of key %s is followed by +." key)
        (goto-char (plist-get prop :begin))
        (kill-region (point) (plist-get prop :end)))
    (goto-char 1))
      (insert "#+" property ": " key " " value "\n"))))

;; --------------

(require 'klin-org-noter)

;; --------

(provide 'klin-optional)
;;; klin-optional.el ends here
