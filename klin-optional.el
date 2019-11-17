;; klin-optional.el --- klin-optional.el                      -*- lexical-binding: t; -*-

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
(require 'klin-conventions)

(require 'cl-lib)

;; ---------- org-mode getting and setting global document properties
;; thanks to Tobias' answer at https://emacs.stackexchange.com/a/21472

;; -------- file add watcher in scanner
(require 'filenotify)



(defvar my-open-the-annotated-version-first t
  "Normally, the annotated version is not being opened first.
But it now can be set to be opened first.")

(defvar desc-global nil
  "Descriptor for the callback.")

(defun klin-watch-callback (event)
  "Insert org link (filepath inside EVENT) to file changed in cloud."
  (interactive)
  (let* ((filepath (expand-file-name (nth 2 event))))
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
    ))

(defvar cloud-scanner-folder
  (expand-file-name "~/Dropbox/1LinkedApps/scanner/"))

(defun klin-org-watch-and-insert-scanned-file ()
  "Launch file system watcher."
  (interactive)
  (let* (desc)
    (if (eq major-mode 'org-mode)
        (progn
          (setq desc (file-notify-add-watch cloud-scanner-folder
                                            '(change attribute-change)
                                            'klin-watch-callback))
          (setq desc-global desc)
          (message "watching for changes in %s, descriptor: %S"
                   cloud-scanner-folder desc))
      (message "Not in org-mode, not setting up a watcher!."))))

(defun abort-watcher-single ()
  "This terminates (deletes) the single watcher."
  (interactive)
  (if desc-global
      (let* ()
        ("Aborting single watcher")
        (file-notify-rm-watch desc-global))
    (message "Nothing to end here!")))

(defvar desc-global-callback-continuous nil
  "Continuous insertion of file names.")

(defun klin-watch-callback-continuous (event)
  "Insert org link (filepath inside EVENT) to file changed in cloud.
and don't remove until a key has been pressed that removes this callback
manually."
  (interactive)
  (let* ((filepath (expand-file-name (nth 2 event))))
    (if (eq (nth 1 event) 'created)
        (progn
          ;; (message "CREATED recognized!")
          ;; (insert "a")
          (message "%s was added" filepath)
          (if (eq major-mode 'org-mode)
              (let* ()
                (insert (concat "[["
                                (klin-utils-get-reduced-file-path filepath)
                                "]"
                                "[scan:"
                                (file-name-nondirectory filepath)
                                "]]"))
                (insert "\n"))
            (message "not in org-mode, not inserting!"))))
    ;; (if desc-global
    ;;     (file-notify-rm-watch desc-global))
    ))

(defun watch-and-insert-arriving-files ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* (desc)
        (setq desc (file-notify-add-watch cloud-scanner-folder
                                          '(change attribute-change)
                                          'klin-watch-callback-continuous))
        (setq desc-global-callback-continuous desc)
        (message "watching continuously for arriving files in %s, descriptor: %S"
                 cloud-scanner-folder desc))
    (message "Not in org-mode, not setting up a continuous watcher!.")))

(defun abort-continous-watcher ()
  "This terminates (deletes) the continuous watcher."
  (interactive)
  (if desc-global-callback-continuous
      (let* ()
        ("Aborting continuous watcher.")
        (file-notify-rm-watch desc-global-callback-continuous))
    (message "Nothing to end here!")))



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
                                            (concat "pwd ; xournalpp " (prin1-to-string freehand-note-filename))))))

(defun my-start-process-freehand-note-program-pdf-exporter-background (freehand-note-filepath)
  "This works specifically for xournalpp.
The command would be: echo file.xoj | entr xournalpp file.xoj -p file.pdf"
  (let* ((freehand-note-filename (file-name-nondirectory freehand-note-filepath))
         proc
         (freehand-notes-filename-base (file-name-base freehand-note-filename)))
    (setq proc (start-process-shell-command (my-get-freehand-entr-process-name freehand-notes-filename-base)
                                            (my-get-freehand-entr-process-buffer-name freehand-notes-filename-base)
                                            (concat "echo " (prin1-to-string freehand-note-filepath)
                                                    " | entr "
                                                    " xournalpp " (prin1-to-string freehand-note-filepath)
                                                    " -p " (prin1-to-string (my-get-freehand-note-filepath-associated-pdf-filepath freehand-note-filepath)))))))

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

(defun my-run-freehand-notes-program-with-file (&optional link-content)
  "If a link of type freehand is clicked, find the file based on LINK-CONTENT (file name base of the notes file) and launch the program."
  (interactive)
  (if link-content
      (let* ((found-file-path (my-get-file-from-searchpaths link-content))
             ;; proc-freehand-notes
             proc-if-already-running)
        (if (setq proc-if-already-running (car (my-get-process-with-name-if-running (my-get-freehand-process-name link-content))))
            (progn
              ;; focus window by PID wmctrl -l -p | grep 13122 | awk '{ print $1 }' | xargs wmctrl -i -a
              (start-file-process-shell-command "wmctrl-select-window-from-pid"
                                                "wmctrl-select-window-from-pid-buf"
                                                (my-get-command-select-gui-window-from-pid (process-id proc-if-already-running)))
              (message (concat "The process with " link-content " is already running!")))

          (let* (proc-freehand-notes-pdf-exporter)
            (set-process-sentinel (setq proc-freehand-notes-pdf-exporter (my-start-process-freehand-note-program-pdf-exporter-background
                                                                          found-file-path))
                                (lambda (process event)
                                  ;; (princ (format "Process: %s had the event '%s'" process
                                  ;;                event))
                                  ))

            (set-process-sentinel (my-start-process-freehand-note-program found-file-path)
                                  (lambda (process event)
                                    ;; (princ (format "Process: %s had the event '%s'" process
                                    ;;                event))
                                    (kill-process proc-freehand-notes-pdf-exporter))))
          ))
    (user-error "No link-content provided")))

(defun my-open-freehand-notes-assoc-pdf (&optional link-content)
  (interactive)
  ;; if link-content wasn't delivered (through the org link interface), but if
  ;; this function is called manually (or from hydra) when on the link, parse the link under point
  ;; for the link-content
  (unless link-content
    (setq link-content (org-element-property :path (org-element-context))))

  (if (my-get-freehand-note-filepath-associated-pdf-filepath
       link-content)
      (my-open-freehand-generated-pdf link-content)))



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


(defun my-open-cited-book-pdf-file (&optional link-content)
  (interactive)
  (message (concat "hi, " link-content))
  (let* ((bibtexkey link-content)
         (description (buffer-substring-no-properties (org-element-property :contents-begin (org-element-context))
                                                      (org-element-property :contents-end (org-element-context)))))
    (klin-org-open-link (list nil bibtexkey description))))

(require 'org-ref)
(org-add-link-type "cite" #'my-open-cited-book-pdf-file)

(defvar my-freehand-notes-extension "xopp")

(defvar my-annotating-freehand-notes-filename-tag "-ann"
  "If an 'original' pdf (intention is to keep the original unaltered) is being
openend to be annotated with the freehand notes program, the annotating
freehand notes file (and it's generated (exported) pdf file, automatically) get a tag appended
to their filenames")

(defun my-get-timestamp ()
  (format-time-string "%Y-%m-%d--%H-%M-%S"))

(defun my-get-new-freehand-note-filename-unique-base-component ()
  "Essentially a timestamp."
  (format-time-string "%Y-%m-%d--%H-%M-%S"))

(defun my-get-freehand-note-annotating-filename-base (original-file-name-base)
  (concat original-file-name-base
          my-annotating-freehand-notes-filename-tag)
  )

(defun my-get-freehand-note-annotating-filename (original-file-name-base)
  "Get filename base based off
- the base name of the original file
- and add the 'annotating' tag to it."
  (concat (my-get-freehand-note-annotating-filename-base original-file-name-base)
          "." my-freehand-notes-extension))

(defun my-get-new-freehand-note-filename ()
  "Get filename base based off timestamp."
  (concat my-get-new-freehand-note-filename-unique-base-component "." my-freehand-notes-extension))

(defun my-get-freehand-note-filepath-associated-pdf-filepath (freehand-note-filepath)
  "Get the assoc pdf file path."
  (concat (file-name-directory freehand-note-filepath) (file-name-base freehand-note-filepath) ".pdf"))

(defun my-get-freehand-note-filepath-from-annotated-pdf-filepath (annotated-pdf-filepath)
  (concat (file-name-sans-extension annotated-pdf-filepath)
          "."
          my-freehand-notes-extension))

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
ppFIXME: automatically terminate the entr session after closing xournal."
  (concat " echo " freehand-note-filepath " | " " entr "
          " xournalpp " freehand-note-filepath " -p "
          (my-get-freehand-note-filepath-associated-pdf-filepath
           freehand-note-filepath)
          " & "))

(defun my-is-filepath-annotating-p (pdf-filepath)
  "Check if the PDF-FILEPATH's base ends with -ann."
  (string-equal (car (last (split-string (file-name-base pdf-filepath) "-")))
                "ann"))

(defun my-is-filepath-generating-p (pdf-filepath)
  "PDF-FILEPATH is generating, if there exists a corresponding -ann file for it."
  (file-exists-p (concat (file-name-directory pdf-filepath)
                         (my-get-freehand-note-annotating-filename (file-name-nondirectory pdf-filepath)))))

(defun my-get-filepath-base-generating (annotated-pdf-filepath)
  ;; if you end in -ann (actually not even necessarily, because every annotated pdf may have a generating pdf
  ;; no matter the name), then deliver back the version with the .ann stripped off
  (let* ((pdf-filepath annotated-pdf-filepath)
         pdf-filepath-without-ann
         (length-of-original-sans-extension (length (file-name-sans-extension annotated-pdf-filepath))))
    (if (string-equal (car (last (split-string my-annotating-freehand-notes-filename-tag "-"))) (car (last (split-string (file-name-base annotated-pdf-filepath) "-"))))
        (setq pdf-filepath-without-ann (substring-no-properties (file-name-sans-extension annotated-pdf-filepath) 0 (- length-of-original-sans-extension (length my-annotating-freehand-notes-filename-tag)))))))

(defun my-is-point-over-link ()
  "Checks if point is over link."
  (eq 'link (car (org-element-context))))

;; (defun my-make-freehand-program-proc-and-proc-buf-name ()
;;   (list (make-temp-name "freehand-proc-")
;;         ))

;; TODO: figure out why this function is still there
(defun klin-org-noter-create-new-xournalpp-note (&optional insert-link-p)
  (interactive)
  (let* ((new-freehand-note-filename (my-get-new-freehand-note-filename))
         (link-content (file-name-base new-freehand-note-filename)))
    (if (not (file-exists-p new-freehand-note-filename))
        (if (and insert-link-p
                 (not (my-is-point-over-link)))
            (progn
              ;; insert a link containing the name base of the freehand note file
              (insert (concat "[[freehand:"
                              link-content
                              "]["
                              link-content
                              "]]"))

              ;; copy the template to filepath
              (copy-file (my-get-freehand-note-template-file-path)
                         new-freehand-note-filename)
              (my-run-freehand-notes-program-with-file link-content))
          (user-error "Point is over link"))
      (user-error "File already exist."))))


(require 'klin-xournalpp)

(defun my-create-new-freehand-note (&optional arg original-filepath)
  "ARG = ann -> create annotating freehand notes file (annotating ANN-PDF)
ARG = nil -> create plain freehand notes file from standard template and
with a standard unique file name"
  (interactive)
  (let* (new-freehand-note-filename)
    (if (not arg)
        (progn
          (setq new-freehand-note-filename (my-get-new-freehand-note-filename))
          (if (not (file-exists-p new-freehand-note-filename))
              (progn
                ;; copy the template to filepath
                (copy-file (my-get-freehand-note-template-file-path)
                           new-freehand-note-filename)
                (setq link-content (file-name-base new-freehand-note-filename))
                (my-run-freehand-notes-program-with-file link-content)
                t)
            (message "File already exists"))))

    (if (eq arg 'ann)
        (if (string-equal (file-name-extension buffer-file-name) "pdf")
            (let* ((original-filename-base (file-name-nondirectory (file-name-base original-filepath))))
              (setq new-freehand-note-filename (my-get-freehand-note-annotating-filename
                                                original-filename-base))
              (if (not (file-exists-p new-freehand-note-filename))  ; this checks only if it's in the current path
                  (progn
                    (setq link-content (file-name-base new-freehand-note-filename))
                    ;; Annotating means: you have already a pdf open, use this as a background
                    ;; for an annotated copy of this same pdf.
                    ;; ;; cp original.pdf original-ann.pdf && xournalpp original-ann.pdf -n 4
                    ;; (copy-file (my-get-freehand-note-template-file-path)
                    ;;            new-freehand-note-filename)
                    (cs-create-xournalpp-file-for-pdf (buffer-file-name)
                                                      (expand-file-name new-freehand-note-filename))

                    ;; TODO: add page degree of freedom to link
                    (my-run-freehand-notes-program-with-file link-content)
                    t)
                (message "File already exists")))))))

;; --------

(require 'cl-lib)

(defun org-global-props-key-re (key)
  "Construct a regular expression matching key and an optional plus and eating the spaces behind.
Test for existence of the plus: (match-beginning 1)"
  (concat "^" (regexp-quote key) "\\(\\+\\)?[[:space:]]+"))

(defun org-global-props (&optional buffer)
  "Get the plists of global org properties of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-equal (org-element-property :key el) "PROPERTY") (nth 1 el))))))

(defun org-global-prop-value (key)
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

(defun org-global-prop-set (key value)
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
      (insert "#+PROPERTY: " key " " value "\n"))))


(defun org-global-keyword-key-value-set (keyword key value)
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
      (insert "#+" keyword ": " key " " value "\n"))))

(defun org-global-keyword-add-set (keyword key value)
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


(require 'pdf-tools)
(require 'pdf-view)

(defun pdf-view-set-comfortable-reading-size ()
  "Set a comfortable reading size.
To be called inside a pdf-view-mode buffer.
Sometimes, the pdf opens up massively downscaled or upscaled.
Then, run this function to adjust."
  (interactive)
  (if (string-equal (file-name-extension (buffer-file-name)) "pdf")
      (let* ((pdf-file-path (buffer-file-name))
             (window-size-width (window-body-width nil t))
             (screen-size-width (x-display-pixel-width))
             pdf-first-page-width-pixels
             (pdfinfo-command (concat "pdfinfo "
                                      (prin1-to-string pdf-file-path)
                                      " | grep \"Page size\" | awk '{ print $3; print $5 }'"))
             shell-command-output
             pdf-width-height-tuple
             document-size-width
             document-size-height
             view-size-width
             (x (/ 1.0 1.618)))

        (pdf-view-scale-reset)

        (setq view-size-width (* x screen-size-width))
        (if (> view-size-width window-size-width)
            (setq pdf-view-display-size 'fit-width)
          (progn
            (setq shell-command-output (shell-command-to-string pdfinfo-command))
            (setq pdf-width-height-tuple (mapcar (lambda (elem)
                                                   (string-to-number elem))
                                                 (split-string shell-command-output "\n")))
            (setq document-size-width (float (nth 0 pdf-width-height-tuple)))
            (setq document-size-height (float (nth 1 pdf-width-height-tuple)))
            (setq pdf-view-display-size (/ (* x screen-size-width) document-size-width))))

        ;; it needs 0.1 s if pdf-view-mode is just started and this function is called as a hook
        ;; if called manually, after pdf-view-mode is fully rendered in initialized, it needs
        ;; basically 0 time
        ;; actually, the only way of starting it should be manually
        (run-with-idle-timer 0.01 nil 'my-run-after-pdf-view-mode-is-ready)
        )))

(defun my-run-after-pdf-view-mode-is-ready ()
  (interactive)
  (when (string-equal "pdf" (file-name-extension (buffer-file-name)))
          (pdf-view-redisplay (selected-window))))

;; ------------------

(defun clone-indirect-buffer-other-frame (newname display-flag &optional norecord)
  "Like `clone-indirect-buffer' but display in another window."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
     (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
           (read-buffer "Name of indirect buffer: " (current-buffer)))
       t)))
  ;; (let ((pop-up-windows t))
  (let ((pop-up-frames t)) ; <==========
    (clone-indirect-buffer newname display-flag norecord)))

(defun get-org-mode-cloning-parameters ()
  (let* ((cloning-params '()))
    ;; window-start
    (push (window-start) cloning-params)))

(defun get-pdf-view-mode-cloning-parameters ()
  (let* ((cloning-params '()))
    ;; bookmark record
    (push (pdf-view-bookmark-make-record) cloning-params))
  )

(defun set-org-mode-cloning-parameters (org-mode-cloning-params)
  (if org-mode-cloning-params
      (let* ()
        ;; window-start
        (set-window-start (selected-window) (nth 0 org-mode-cloning-params)))))

(defun set-pdf-view-mode-cloning-parameters (cloning-params)
  (if cloning-params
      (let* (;; the bookmark record includes as the buffer name as car
             ;; so: replace the old buffer name with the new one in the bookmark record
             (bmk-orig (car cloning-params))
             (bmk-adapted (push (buffer-name) (cdr bmk-orig))))
        ;; bookmark record
        ;; (revert-buffer nil t)
        (pdf-view-mode)
        (pdf-view-redisplay-pages)
        (pdf-view-bookmark-jump bmk-adapted) ;; doesn't work on clones for some reason

        )))


(defun get-proper-mode-cloning-parameters ()
  (cond
   ((string-equal (file-name-extension (buffer-file-name)) "org")
    (get-org-mode-cloning-parameters))
   ((string-equal (file-name-extension (buffer-file-name)) "pdf")
    (get-pdf-view-mode-cloning-parameters))
   ))

(defun set-proper-mode-cloning-parameters (cloning-params)
  (cond
   ((string-equal (file-name-extension (buffer-file-name)) "org")
    (set-org-mode-cloning-parameters cloning-params))
   ((string-equal (file-name-extension (buffer-file-name)) "pdf")
    (set-pdf-view-mode-cloning-parameters cloning-params))
   ))

(defun klin-clone-into-new-frame ()
  (interactive)
  (let* ((cur-buf (current-buffer))
         (cloning-params (get-proper-mode-cloning-parameters))
         (new-frame (make-frame `((width . (text-pixels . ,(window-size nil t t)))
                                  (height . (text-pixels . ,(window-size nil nil t)))
                                  (left . ,(+ (car (frame-position)) (window-pixel-left)))
                                  (top . ,(+ (cdr (frame-position)) (window-pixel-top))))))
         (new-window (car (window-list new-frame)))
         new-buffer)
    (with-selected-window new-window
      (setq new-buffer (make-indirect-buffer cur-buf
                                             (generate-new-buffer-name (buffer-name cur-buf))
                                             nil))

      ;; (setq clone-buffer (clone-indirect-buffer (buffer-file-name) t))
      (if (not (equal (current-buffer) new-buffer))
          (switch-to-buffer new-buffer nil t))

      ;; set buffer-file-name (mostly, that's useful e.g. for saving ability)
      (with-current-buffer new-buffer
        (setq buffer-file-name (buffer-file-name cur-buf)))
      (with-current-buffer new-buffer
        (set-proper-mode-cloning-parameters cloning-params)))))


;; override this, because if you launch this function from a clone,
;; it actually jumps to the bookmark in the original pdf
;; so, make it check first if the buffer (clone buffer) you are on
;; when running this function, is visiting the file (pdf)
(require 'pdf-view)
(defun pdf-view-bookmark-jump (bmk)
  "Switch to bookmark BMK.

This function is like `bookmark-jump', but it always uses the
selected window for display and does not run any hooks.  Also, it
works only with bookmarks created by
`pdf-view-bookmark-make-record'."

  (let* ((file (bookmark-prop-get bmk 'filename))
         ;; return the current buffer if it is visiting the file
         (buffer (or (when (string-equal (expand-file-name (buffer-file-name))
                                         (expand-file-name file))
                       (current-buffer))
                     (find-buffer-visiting file)
                     (find-file-noselect file))))
    (switch-to-buffer buffer)
    (let (bookmark-after-jump-hook)
      (pdf-view-bookmark-jump-handler bmk)
      (run-hooks 'bookmark-after-jump-hook))))

(defun pdf-view-bookmark-jump-handler (bmk)
  "The bookmark handler-function interface for bookmark BMK.

See also `pdf-view-bookmark-make-record'."
  (let ((page (bookmark-prop-get bmk 'page))
        (slice (bookmark-prop-get bmk 'slice))
        (size (bookmark-prop-get bmk 'size))
        (origin (bookmark-prop-get bmk 'origin))
        (file (bookmark-prop-get bmk 'filename))
        (show-fn-sym (make-symbol "pdf-view-bookmark-after-jump-hook")))
    (fset show-fn-sym
          (lambda ()
            (remove-hook 'bookmark-after-jump-hook show-fn-sym)
            (unless (derived-mode-p 'pdf-view-mode)
              (pdf-view-mode))
            (with-selected-window
                (or (get-buffer-window (current-buffer) 0)
                    (selected-window))

              (message (concat "this buffer: "
                                 (prin1-to-string (current-buffer))
                                 "this window: "
                                 (prin1-to-string (selected-window))))
              (when size
                (setq-local pdf-view-display-size size))
              (when slice
                (apply 'pdf-view-set-slice slice))
              (when (numberp page)
                (pdf-view-goto-page page))
              (when origin
                (let ((size (pdf-view-image-size t)))
                  (image-set-window-hscroll
                   (round (/ (* (car origin) (car size))
                             (frame-char-width))))
                  (image-set-window-vscroll
                   (round (/ (* (cdr origin) (cdr size))
                             (frame-char-height)))))))))
    (add-hook 'bookmark-after-jump-hook show-fn-sym)
    (set-buffer (or (when (string-equal (expand-file-name (buffer-file-name))
                                         (expand-file-name file))
                       (current-buffer))
                    (find-buffer-visiting file)
                    (find-file-noselect file)))))

(defun get-two-files-and-ask-merge ()
  "Select two org links by marking the region.
Then run this function to extract the two file paths and
suggest a command to be run."
  (interactive)
  (if (not (region-active-p))
      (user-error "You must select a region."))

  (let* ((region-substr (buffer-substring-no-properties (region-beginning)
                                                        (region-end)))
         (links '())
         cmd-str
         (output-pdf-filename (concat "merged-" (my-get-timestamp) ".pdf"))
         equal-directory-path
         output-file-dir-path (expand-file-name "./")
         output-filepath)
    (with-temp-buffer
      (let* (point-recent)
        (insert region-substr)
        (org-mode)
        (goto-char (point-min))
        (org-next-link)
        (setq point-recent (point))
        (setq links (append (list (expand-file-name (org-element-property :path (org-element-context))))
                            links))

        (org-next-link)
        (if (eq (point) point-recent)
            (user-error (concat "Please select two links! here: links="
                            (prin1-to-string links))))
        (setq links (append (list (expand-file-name (org-element-property :path (org-element-context))))
                            links))))
    ;; now contruct the command; template: pdftk A=odd.pdf B=even.pdf shuffle A B output collated_pages.pdf

    (if (not (= (length links) 2))
        (user-error (concat "Please select two links! here: links="
                            (prin1-to-string links))))

    (if (string-equal (setq equal-directory-path (file-name-directory (nth 0 links))) (file-name-directory (nth 1 links)))
        (setq output-file-dir-path (expand-file-name equal-directory-path))
      (setq equal-directory-path nil)
      (setq output-file-dir-path (expand-file-name "./")))

    (setq output-filepath (concat output-file-dir-path output-pdf-filename))
    (setq cmd-str (concat "pdftk A="
                          (prin1-to-string (nth 0 links))
                          " B="
                          (prin1-to-string (nth 1 links))
                          " shuffle A B output "
                          (prin1-to-string (expand-file-name output-filepath))))

    (setq cmd-str (read-string "Run the command: " cmd-str))
    (shell-command-to-string cmd-str)
    (if (file-exists-p output-filepath)
        (progn
          (unless (string-equal ""
                                (save-excursion
                                  (buffer-substring-no-properties (progn
                                                                    (beginning-of-line)
                                                                    (point))
                                                                  (progn
                                                                    (end-of-line)
                                                                    (point)))))
            (insert "\n"))
          (insert (with-temp-buffer
                    (org-mode)
                    (org-insert-link t
                                     output-filepath
                                     (file-name-nondirectory output-filepath))
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
          (insert "\n")))
    ))


(provide 'klin-optional)
;;; klin-optional.el ends here
