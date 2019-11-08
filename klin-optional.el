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


(defvar my-freehand-note-format-file-extension "xopp")

(defun my-get-freehand-notes-filename-from-file-name-base (file-name-base)
  (concat file-name-base
          "."
          my-freehand-note-format-file-extension
          ))


(defun my-start-process-freehand-note-program (freehand-note-filepath)
  (let* ((freehand-note-filename (file-name-nondirectory freehand-note-filepath))
         proc
         (freehand-notes-filename-base (file-name-base freehand-note-filename)))
    (setq proc (start-process-shell-command (my-get-freehand-process-name freehand-notes-filename-base)
                                            (my-get-freehand-process-buffer-name freehand-notes-filename-base)
                                            ;; (my-command-to-launch-freehand-program new-freehand-note-filename)
                                            (concat "pwd ; xournalpp " freehand-note-filename)))))

(defun my-start-process-freehand-note-program-pdf-exporter-background (freehand-note-filepath)
  "This works specifically for xournalpp.
The command would be: echo file.xoj | entr xournalpp file.xoj -p file.pdf"
  (let* ((freehand-note-filename (file-name-nondirectory freehand-note-filepath))
         proc
         (freehand-notes-filename-base (file-name-base freehand-note-filename)))
    (setq proc (start-process-shell-command (my-get-freehand-entr-process-name freehand-notes-filename-base)
                                            (my-get-freehand-entr-process-buffer-name freehand-notes-filename-base)
                                            (concat "echo " freehand-note-filepath
                                                    " | entr "
                                                    " xournalpp " freehand-note-filepath
                                                    " -p " (my-get-freehand-note-fliepath-associated-pdf-filepath
                                                            freehand-note-filepath))))))

(defun path-is-relative (path)
  "Works on linux."
  (not (string-equal (substring-no-properties path 0 1) "/")))

(defun my-get-freehand-process-name (link-content)
  (concat "freehand-proc-" link-content))

(defun my-get-freehand-process-buffer-name (link-content)
  (concat "freehand-buf-" link-content))

(defun my-get-freehand-entr-process-name (link-content)
  (concat "freehand-entr-proc-" link-content))

(defun my-get-freehand-entr-process-buffer-name (link-content)
  (concat "freehand-entr-buf-" link-content))

(defvar freehand-notes-searchpaths (list "./"))


(defun my-get-process-with-name-if-running (proc-name)
  "Get the process back with it's name matching PROC-NAME."
  (remove nil
          (mapcar (lambda (elem)
                    (let* ((pname (nth 0 elem))
                           (p (nth 1 elem)))
                      (when (string-match-p (regexp-quote proc-name)
                                            pname)
                        p)))
                  (mapcar (lambda (process)
                            ;; return the name and the process itself
                            (list (process-name process) process))
                          (process-list)))))

(defun my-process-get- (pid)
  "Return the pid of the process "
  (remove nil
          (mapcar (lambda (elem)
                    (when (eq elem pid)
                      elem))
                  (mapcar (lambda (process)
                            (process-id process))
                          (process-list)))))

(defun my-get-command-select-gui-window-from-pid (pid)
  (concat "wmctrl -l -p | grep " (number-to-string pid) " | awk '{ print $1 }' | xargs wmctrl -i -a"))

(defun my-run-freehand-notes-program-with-file (link-content)
  "If a link of type freehand is clicked, find the file based on LINK-CONTENT (file name base of the notes file) and launch the program."
  (interactive)
  (let* ((found-file-path (my-get-file-from-searchpaths link-content))
         proc-freehand-notes)
    (if (setq proc-if-already-running (car (my-get-process-with-name-if-running (my-get-freehand-process-name link-content))))
        (progn
          ;; focus window by PID wmctrl -l -p | grep 13122 | awk '{ print $1 }' | xargs wmctrl -i -a
          (start-file-process-shell-command "wmctrl-select-window-from-pid"
                                            "wmctrl-select-window-from-pid-buf"
                                            (my-get-command-select-gui-window-from-pid (process-id proc-if-already-running)))
          (message (concat "The process with " link-content " is already running!")))


      (set-process-sentinel (my-start-process-freehand-note-program found-file-path)
                            (lambda (process event)
                              (princ (format "Process: %s had the event '%s'" process
                                             event))
                              (kill-process proc-freehand-notes-pdf-exporter)))
      (set-process-sentinel (my-start-process-freehand-note-program-pdf-exporter-background
                             found-file-path)
                            (lambda (process event)
                              (princ (format "Process: %s had the event '%s'" process
                                             event)))))))

(defun my-get-file-from-searchpaths (file-name-base)
  "Go through the search paths and find the 1st occurence of the file with FILE-NAME-BASE."
  (let* ((ctr 0)
         (base-of-relative-searchpaths (file-name-directory (buffer-file-name)))
         ;; take the first one as the found file path
         matching-candidate-expanded-filepath
         current-candidate-expanded-filepath
         relative-search-path-expanded-dir
         current-searchpath)
    (while (< ctr (length freehand-notes-searchpaths))
      (setq current-candidate-expanded-filepath nil)
      (setq current-searchpath (file-name-as-directory (nth ctr freehand-notes-searchpaths)))
      ;; test if it's relative or absolute, then return it's full expanded form
      (if (path-is-relative current-searchpath)
          (when (file-exists-p (setq current-candidate-expanded-filepath (expand-file-name (concat base-of-relative-searchpaths
                                                                                                   current-searchpath
                                                                                                   (my-get-freehand-notes-filename-from-file-name-base
                                                                                                    file-name-base)))))
            (setq matching-candidate-expanded-filepath
                  current-candidate-expanded-filepath))
        ;; otherwise, it's an absolute searchpath
        (when (file-exists-p (setq current-candidate-expanded-filepath (expand-file-name (concat current-searchpath
                                                                                                 (my-get-freehand-notes-filename-from-file-name-base
                                                                                                  file-name-base)))))
          (setq matching-candidate-expanded-filepath
                current-candidate-expanded-filepath)))
      (when matching-candidate-expanded-filepath
        (setq ctr (length freehand-notes-searchpaths)))
      (setq ctr (+ ctr 1)))
    matching-candidate-expanded-filepath))

(org-add-link-type "freehand" #'my-run-freehand-notes-program-with-file)

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

(defun my-command-to-launch-freehand-program (what-file-path)
  (interactive)
  (concat "xournalpp " what-file-path " & "))

(defun my-command-to-launch-background-auto-pdf-exporter-for-freehand-note (freehand-note-filepath)
  "Entr, export automatically to PDF.
FIXME: automatically terminate the entr session after closing xournal."
  (concat " echo " freehand-note-filepath " | " " entr "
          " xournalpp " freehand-note-filepath " -p "
          (my-get-freehand-note-fliepath-associated-pdf-filepath
           freehand-note-filepath)
          " & "))

(defun my-is-point-over-link ()
  "Checks if point is over link."
  (eq 'link (car (org-element-context))))

;; (defun my-make-freehand-program-proc-and-proc-buf-name ()
;;   (list (make-temp-name "freehand-proc-")
;;         ))

(defun klin-org-noter-create-new-xournalpp-note-and-insert-link ()
  (interactive)
  (let* ((new-freehand-note-filename (my-get-new-freehand-note-filename)))
    (if (not (file-exists-p new-freehand-note-filename))
        (if (not (my-is-point-over-link))
            (progn
              ;; insert a link containing the name base of the freehand note file
              (insert (concat "[[freehand:"
                              (file-name-base new-freehand-note-filename)
                              "]["
                              (file-name-base new-freehand-note-filename)
                              "]]"))

              ;; copy the template to filepath
              (copy-file (my-get-freehand-note-template-file-path)
                         new-freehand-note-filename)
              (visit-freehand-link (file-name-base new-freehand-note-filename)))
          (user-error "Point is over link"))
      (user-error "File already exist."))))

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
