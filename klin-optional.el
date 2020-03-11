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
                (insert "\n")
                ;; (when (y-or-n-p-with-timeout "Abort watching? "
                ;;                              5 nil )
                ;;   (quit-watch))
                )
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


(defun quit-watch ()
  (interactive)
  (when desc-global-callback-continuous
    (message "Aborting continuous watcher.")
    (file-notify-rm-watch desc-global-callback-continuous))
  (when desc-global
    (message "Aborting single watcher.")
    (file-notify-rm-watch desc-global)))

(defun pdf-link-pdf-reverse-page-order ()
  (interactive)
  (let* ((file-path (expand-file-name (org-element-property :path (org-element-context)))))
    (pdf-reverse-page-order file-path)
    ))

(defun pdf-reverse-page-order (file-path)
  (shell-command-to-string (read-shell-command "Run 'Reverse PDF page order' command: "
                                                 (concat "pdfjam "
                                                         (prin1-to-string file-path)
                                                         " 'last-1' --outfile "
                                                         (prin1-to-string file-path)))))

(defun pdf-merge-two-pdfs-from-scan-from-my-printer ()
  "I scan pages from my printer like so: all even pages -> 1 pdf file, all odd pages, reverse order -> 1 pdf file.  To collate them properly, I have to first act on the 2nd file, an org file link, and reverse the PDF file's page order.  Then I run collate command on both files."
  (interactive)
  (save-excursion
    ;; get 2nd file
    (let* ((all-org-mode-links (find-all-org-mode-links-in-selected-region))
           first-link
           second-link)
      (if (equal (length all-org-mode-links) 2)
          (progn
            (setq first-link (nth 0 all-org-mode-links))
            (setq second-link (nth 1 all-org-mode-links))
            (if (string-equal first-link second-link)
                (unless (yes-or-no-p "Hold on, ... they are equal links! Continue anyway?")
                  (user-error "Aborted")))

            (if (region-active-p)
                (progn
                  ;; reverse page order on the 2nd one
                  (pdf-reverse-page-order second-link)
                  ;; continue to merge them
                  (get-two-files-and-ask-merge)
                  )))
        (user-error "Please select two org-mode file links")))))

(defun my-get-freehand-notes-filename-from-file-name-base (file-name-base)
  (concat file-name-base "." my-freehand-note-format-file-extension))

(defun my-start-process-freehand-note-program (freehand-note-filepath)
  (let* ((freehand-note-filename (file-name-nondirectory freehand-note-filepath))
         proc
         (freehand-notes-filename-base (file-name-base freehand-note-filename)))
    (setq proc (start-process-shell-command (my-get-freehand-process-name freehand-notes-filename-base)
                                            (my-get-freehand-process-buffer-name freehand-notes-filename-base)
                                            ;; (my-command-to-launch-freehand-program new-freehand-note-filename)
                                            (concat "pwd ; xournalpp " (prin1-to-string freehand-note-filename))))))

(defun pdf-view--rotate (&optional counterclockwise-p page-p)
  "Rotate PDF 90 degrees.  Requires pdftk to work.\n
Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
non-nil for the other direction.  Rotate the whole document by
default; set PAGE-P to non-nil to rotate only the current page.
\nWARNING: overwrites the original file, so be careful!"
  ;; error out when pdftk is not installed
  (if (null (executable-find "pdftk"))
      (error "Rotation requires pdftk")
    ;; only rotate in pdf-view-mode
    (when (eq major-mode 'pdf-view-mode)
      (let* ((rotate (if counterclockwise-p "left" "right"))
             (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
             (page   (pdf-view-current-page))
             (pages  (cond ((not page-p)                        ; whole doc?
                            (format "1-end%s" rotate))
                           ((= page 1)                          ; first page?
                            (format "%d%s %d-end"
                                    page rotate (1+ page)))
                           ((= page (pdf-info-number-of-pages)) ; last page?
                            (format "1-%d %d%s"
                                    (1- page) page rotate))
                           (t                                   ; interior page?
                            (format "1-%d %d%s %d-end"
                                    (1- page) page rotate (1+ page))))))
        ;; empty string if it worked
        (if (string= "" (shell-command-to-string
                         (format (concat "pdftk %s cat %s "
                                         "output %s.NEW "
                                         "&& mv %s.NEW %s")
                                 file pages file file file)))
            (pdf-view-revert-buffer nil t)
          (error "Rotation error!"))))))

(defun pdf-view-rotate-clockwise (&optional arg)
  "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
entire document."
  (interactive "P")
  (pdf-view--rotate nil (not arg)))

(defun pdf-view-rotate-counterclockwise (&optional arg)
  "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
rotate entire document."
  (interactive "P")
  (pdf-view--rotate :counterclockwise (not arg)))

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

(defun org-link-get-description ()
  "With point on a link, get it's description."
  (let* ((pos1 (org-element-property :contents-begin (org-element-context)))
         (pos2 (org-element-property :contents-end (org-element-context))))
    (when (and pos1 pos2)
      (buffer-substring-no-properties pos1 pos2))))

(defun my-test ()
  "Testing separating cite:mykey::123::p234"
  (let* ((link-str "mykey::123::p234::ms=awefawef aewfewa aew::mso=3")
         ;; (link-type (let* (result)
         ;;              (string-match "\\`\\(.+?\\):" link-str)
         ;;              (setq result (match-string 1 link-str))))
         (bibtex-key (let* (result)
                       (string-match "\\`\\(.+?\\)\\(\\'\\|:\\)"
                                     link-str)
                       (setq result (match-string 1 link-str))))
         (pdf-page (let* (result)
                     (string-match "::\\([0-9]+\\)" link-str)
                     (setq result (match-string 1 link-str))
                     (when result
                       (string-to-number result))))
         (doc-page (let* (result)
                     (string-match "::p\\([0-9]+\\)" link-str)
                     (setq result (match-string 1 link-str))
                     (when result
                       (string-to-number result))))
         (marked-str (let* (result)
                       (string-match "::ms=\\(.+?\\)\\(\\'\\|:\\)"
                                     link-str)
                       (setq result (match-string 1 link-str))))
         (marked-str-occ (let* (result)
                           (string-match "::mso=\\([0-9]+?\\)\\(\\'\\|:\\)"
                                         link-str)
                           (setq result (match-string 1 link-str))
                           (when result
                             (string-to-number result)))))))

(defun klin-get-assoc-list-from-cite-link-str (link-str)
  `( ;; (link-type . ,(let* (result)
    ;;                 (string-match "\\`\\(.+?\\):" link-str)
    ;;                 (setq result (match-string 1 link-str))))
    (bibtex-key . ,(let* (result)
                     (string-match "\\`\\(.+?\\)\\(\\'\\|:\\)"
                                   link-str)
                     (setq result (match-string 1 link-str))))
    (pdf-page . ,(let* (result)
                   (when (string-match "::\\([0-9]+\\)" link-str)
                     (setq result (match-string 1 link-str))
                     (when result
                       (string-to-number result)))))
    (doc-page . ,(let* (result)
                   (when (string-match "::p\\([0-9]+\\)" link-str)
                     (setq result (match-string 1 link-str))
                     (when result
                       (string-to-number result)))))
    (marked-str . ,(let* (result)
                     (when (string-match "::ms=\\(.+?\\)\\(\\'\\|:\\)"
                                         link-str)
                       (setq result (match-string 1 link-str)))))
    (marked-str-occ . ,(let* (result)
                         (when (string-match "::mso=\\([0-9]+?\\)\\(\\'\\|:\\)"
                                             link-str)
                           (setq result (match-string 1 link-str))
                           (when result
                             (string-to-number result)))))
    (description . ,(org-link-get-description))))

(defun my-open-cited-book-pdf-file (&optional link-str)
  "Dissect the LINK-STR."
  (interactive)
  (klin-org-open-link (klin-get-assoc-list-from-cite-link-str link-str)))

(defun my-get-org-link-at-point-cited-reference-pdf-file-path ()
  (interactive)
  (let* ((bibtexkey (org-element-property :path (org-element-context)))
         (description (org-link-get-description)))
    (klin-org-get-link-filepath (list nil bibtexkey description))))

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
  (concat (my-get-new-freehand-note-filename-unique-base-component) "." my-freehand-notes-extension))

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
                (if (yes-or-no-p "Create file and insert link?")
                    (progn
                      ;; copy the template to filepath
                      (copy-file (my-get-freehand-note-template-file-path)
                                 new-freehand-note-filename)
                      (setq link-content (file-name-base new-freehand-note-filename))
                      (my-run-freehand-notes-program-with-file link-content)
                      ;; insert the link
                      (insert (concat "[[freehand:" link-content "]]"))
                      t)))
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
             scale-factor
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

            ;; if it's not a4, just brute-force it to be a4
            (setq document-size-height (float (nth 1 pdf-width-height-tuple)))

            ;; if it's not a4, just brute-force it to be a4, i.e. desired width is 476
            (setq scale-factor (/ document-size-width (* 1.2 476.0)))
            (setq scale-factor (/ 1.0 scale-factor))
            (setq pdf-view-display-size (/ (* x screen-size-width)
                                           (* scale-factor document-size-width)))))

        ;; it needs 0.1 s if pdf-view-mode is just started and this function is called as a hook
        ;; if called manually, after pdf-view-mode is fully rendered in initialized, it needs
        ;; basically 0 time
        ;; actually, the only way of starting it should be manually
        (run-with-idle-timer 0.01 nil 'my-run-after-pdf-view-mode-is-ready)
        (pdf-view-redisplay)
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

(defun make-frame-same-dim-as-cur-window ()
  (interactive)
  (make-frame `((width . (text-pixels . ,(window-size nil t t)))
                                  (height . (text-pixels . ,(window-size nil nil t)))
                                  (left . ,(+ (car (frame-position)) (window-pixel-left)))
                                  (top . ,(+ (cdr (frame-position)) (window-pixel-top))))))

(defun klin-clone-into-new-frame ()
  (interactive)
  (let* ((cur-buf (current-buffer))
         (cloning-params (get-proper-mode-cloning-parameters))
         (new-frame (make-frame-same-dim-as-cur-window))
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

(defun klin-clone-into-split-window ()
  (interactive)
  (let* ((cur-buf (current-buffer))
         (cloning-params (get-proper-mode-cloning-parameters))
         new-buffer
         (new-window (split-window-sensibly)))

    (if (not new-window)
        (setq new-window (split-window-below)))

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
      (user-error "You must select a region"))

  (let* ((links (find-all-org-mode-links-in-selected-region))
         cmd-str
         (output-pdf-filename (concat (my-get-timestamp) ".pdf"))
         equal-directory-path
         output-file-dir-path (expand-file-name "./")
         output-filepath-initially
         output-filepath)
    ;; now contruct the command; template: pdftk A=odd.pdf B=even.pdf shuffle A B output collated_pages.pdf

    (if (not (= (length links) 2))
        (user-error (concat "Please select two links! here: links="
                            (prin1-to-string links))))

    (if (string-equal (setq equal-directory-path (file-name-directory (nth 0 links))) (file-name-directory (nth 1 links)))
        (setq output-file-dir-path (expand-file-name equal-directory-path))
      (setq equal-directory-path nil)
      (setq output-file-dir-path (expand-file-name "./")))

    (setq output-filepath-initially (concat output-file-dir-path output-pdf-filename))
    (let* ((default-command (concat "pdftk A="
                                    (prin1-to-string (nth 0 links))
                                    " B="
                                    (prin1-to-string (nth 1 links))
                                    " shuffle A B output "
                                    (prin1-to-string (expand-file-name output-filepath-initially))))
           (cmd-str (read-shell-command "Collate two PDFs: "
                                        (cons default-command (- (string-bytes default-command)
                                                                 (string-bytes "pdf\""))))))
      ;; get the new output filepath
      (with-temp-buffer
        (insert cmd-str)
        (goto-char (point-min))
        (re-search-forward " output ")
        (setq output-filepath (expand-file-name (read (buffer-substring-no-properties (point)
                                                                                (progn
                                                                                  (end-of-line)
                                                                                  (point)))))))

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
                                       (klin-utils-get-reduced-file-path output-filepath)
                                       (file-name-nondirectory output-filepath))
                      (buffer-substring-no-properties (point-min)
                                                      (point-max))))
            (insert "\n"))))
    ))


;; --------- open pdf in chrome ---------

(defun klin-open-pdf-in-browser ()
  "Open pdf in chrome."
  (interactive)
  (let* ((shell-command-string (read-shell-command "Open PDF in browser: "
                                                   (concat "x-www-browser "
                                                           (prin1-to-string (concat "file://"
                                                                                    (buffer-file-name)
                                                                                    "#page="
                                                                                    (number-to-string (pdf-view-current-page))))
                                                           ;; " &"
                                                           ))))
    (start-process-shell-command "browser-open-cmd-name" "browser-open-cmd-buf" shell-command-string)))


;; ----------- add text search to org link ------------

(require 'org-pdfview)
(org-add-link-type "pdfview" #'pdfview-follow-link #'my-org-pdfview-export)

(defun klin-get-assoc-list-from-pdfview-link-str (link-str)
  "Parse the contents of a pdfview LINK-STR into an association list."
  `((pdf-url . ,(let* (result)
                  (string-match "\\`\\(.+?\\)\\(\\'\\|::\\)"
                                link-str)
                  (setq result (match-string 1 link-str))))
    (pdf-page . ,(let* (result)
                   (when (string-match "::\\([0-9]+\\)" link-str)
                     (setq result (match-string 1 link-str))
                     (when result
                       (string-to-number result)))))
    ;; maybe do also marked string and marked string occurrence
    (description . ,(org-link-get-description)))
  )

(defun my-org-pdfview-export (link description format)
  "Export a pdfview link."
  (let* ((path link)
         (desc (strip-text-properties description))
         (link-info (klin-get-assoc-list-from-pdfview-link-str
                     link))
         (pdf-url (when link-info
                    (let* ((result (cdr (assoc 'pdf-url link-info))))
                      result)))
         (pdf-page (when link-info
                     (let* ((result (cdr (assoc 'pdf-page link-info))))
                       result)))
         (url (org-latex-plain-text (concat pdf-url
                                            "#page="
                                            (if pdf-page
                                                (number-to-string pdf-page)
                                              ""))
                                    nil)))
    (pcase format
      ;; (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      ;; (`latex (format "\\href{%s}{%s}" path desc))
      (`latex
       ;; (format "\\cite[%s]{%s}" desc bibtex-key)
       (concat (if desc
                   (concat (org-latex-plain-text desc nil)
                           "~")
                 nil
                 "")
               "[\\,"
               "\\footnote{"
               (if (file-exists-p pdf-url)
                   (concat (file-name-base pdf-url)
                           (if pdf-page
                               (concat "; "
                                       (number-to-string pdf-page))
                             ""))
                 (format "\\url{%s}" url))
               "}"
               (if (file-exists-p pdf-url)
                   ""
                 (concat ","
                         (format "\\href{%s}{%s}" url "$\\nearrow$")))
               "\\,]"))
      ;; (`texinfo (format "@uref{%s,%s}" path desc))

      (`ascii
       (format "%s (%s)" desc path))
      (_ path))))

(require 'org-pdfview)

;; (defun org-pdfview-convert-to-cite ()
;;   "With cursor on a pdfview link, convert it to a cite link."
;;   (interactive)
;;   ;; first check if there is already a bibtex entry with the filepath (or url) in it linking to
;;   ;; this file. If yes, just use that.
;;   )

(defun org-pdfview-store-link ()
  "Store a link to a pdfview buffer.
This time, include the page, but also a search string on the page, that may be selected.
If you then jump to the link, search for this string on the page."
  (when (eq major-mode 'pdf-view-mode)
    (let* ((path buffer-file-name)
           (page (pdf-view-current-page))
           (link (concat "pdfview:"
                         path
                         "::"
                         (number-to-string page)
                         (when (pdf-view-active-region-p)
                           ;; sth was selected
                           (concat "::"
                                   (string-escape-newlines (car (pdf-view-active-region-text)))
                                   (let* ((match-number (get-match-number-of-selected-text-on-page)))
                                     (when (and match-number
                                                (> match-number 1))
                                       (concat "::"
                                               (number-to-string match-number))))))))
           (description (file-name-nondirectory (buffer-file-name))))
      (org-store-link-props :type "pdfview"
                            :link link
                            :description description))))

(defun string-utils-escape-double-quotes (str-val)
  "Return STR-VAL with every double-quote escaped with backslash."
  (save-match-data
    (replace-regexp-in-string "\"" "\\\\\"" str-val)))

(defun string-escape-newlines (str-with-newlines)
  (when (char-or-string-p str-with-newlines)
    (replace-regexp-in-string (regexp-quote "\n") (regexp-quote "\\n") str-with-newlines)))

(defun string-unescape-newlines (str-with-escaped-newlines)
  (when (char-or-string-p str-with-escaped-newlines)
    (replace-regexp-in-string (regexp-quote "\\n") (regexp-quote "\n") str-with-escaped-newlines)))

(defun normalize-pdf-view-active-region-edges (edges)
  "Get edges of mouse-selected active region, in the right order, meaning
   left1, top1, left2, top2,
   where left1 should be < left2,
         top1 should be < top2.
  If they should be equal, set the second coordinate to be one pixel greater."
  (list (min (nth 0 edges) (nth 2 edges))
        (min (nth 1 edges) (nth 3 edges))
        (if (eq (nth 0 edges) (nth 2 edges))
            (+ (nth 2 edges) 1)
          (max (nth 0 edges) (nth 2 edges)))
        (if (eq (nth 1 edges) (nth 3 edges))
            (+ (nth 3 edges) 1)
          (max (nth 1 edges) (nth 3 edges)))))

(defun get-match-number-of-selected-text-on-page (&optional matches-edges-list selected-edges)
  "Get the mouse-selected rectangle, match it with all the other rectangles from the
  search of the currenlty selected text, figure out a single overlapping rectangle and the
  occurrence number of the selected text on the page. Idiosyncracy: (pdf-view-active-region)
  returns the mouse-dragged edges, not the edges that the renderer displays as a selection,
  after dragging."
  (interactive)
  (when (pdf-view-active-region-p)
    (unless selected-edges
      (setq selected-edges (list (mapcar (lambda (cur-num)
                                           (round cur-num))
                                         (car (pdf-util-scale-relative-to-pixel (pdf-view-active-region)))))))
    (unless matches-edges-list
      (setq matches-edges-list (pdf-isearch-search-page (car (pdf-view-active-region-text)))))
    (let* ((intersection-areas
            (mapcar (lambda (cur-matching-edges)
                      (pdf-util-edges-intersection-area (car cur-matching-edges)
                                                        (normalize-pdf-view-active-region-edges (car selected-edges))))
                    matches-edges-list))
           lia
           liai)
      (when intersection-areas
        (if (remove 0 intersection-areas)
            (progn
              (setq lia (-max intersection-areas))
              (setq liai (car (--find-indices (eq lia it)
                                              intersection-areas)))
              (+ 1 liai))
          (user-error (concat "selected-edges=" (prin1-to-string selected-edges) ", "
                              "matches-edges-list=" (prin1-to-string matches-edges-list) ", "
                              "intersection-areas=" (prin1-to-string intersection-areas) ", "
                              "liai=" (prin1-to-string liai))))))))

(defconst path-of-foo (file-name-directory (or load-file-name buffer-file-name)))

(defun klin-url-opened-in-firefox (url)
  (interactive)
  (car (remove nil (mapcar (lambda (str)
                             (string-equal (cut-hastags-off-path str) (cut-hastags-off-path url)))
                           (split-string (shell-command-to-string firefox-list-open-tabs-script-filename)
                                         "\n")))))

(defun cut-hastags-off-path (url)
  (interactive)
  (car (split-string url "#")))

(defun pdfview-follow-link (link)
  (interactive)
  (let* (;; (link-without-type (substring-no-properties link (length "pdfview:")))
         (splits (split-string link "::"))
         (path (nth 0 splits))
         (page (string-to-number (let* ((page-tmp (nth 1 splits)))
                                   (if page-tmp
                                       page-tmp
                                     "1")))))
    (cond
     ((<= (length splits) 2)
      (cond
       ((file-exists-p path)
        (org-open-file path 1)
        (pdf-view-goto-page page))
       ((klin-is-this-link-to-be-opened-in-a-browser link)
        (when (string-equal (file-name-extension (cut-hastags-off-path path)) "pdf")
          (if (klin-url-opened-in-firefox path)
            (when (y-or-n-p-with-timeout (concat path " is already opened in browser. Open another instance?")
                                         3 nil)
              (browse-url (concat path
                                  "#page="
                                  (number-to-string page))))
            (browse-url (concat path "#page=" (number-to-string page))))))))
     ((> (length splits) 2)
      (let* ((search-str (car (split-string (string-unescape-newlines (nth 2 splits))
                                            "\n")))
             (search-str-occ-num (if (nth 3 splits)
                                     (string-to-number (nth 3 splits))
                                   1)))
        (org-open-file path 1)
        (pdf-view-goto-page page)
        (when search-str
          (let* ((edges (pdf-isearch-search-page search-str))
                 (edges-of-current-match (nth (- search-str-occ-num 1)
                                              edges)))
            (pdf-isearch-hl-matches nil edges t)
            ;; scroll the current match into view
            (pdf-util-scroll-to-edges (apply 'pdf-util-edges-union edges-of-current-match))
            (put-tooltip-arrow edges-of-current-match)
            )))))
    ;; (message link)
    ;; (other-window -1)
    ))

(defun put-tooltip-arrow (edges-of-match)
  (interactive)
  (let* ((pdf-image-height (cdr (pdf-view-image-size)))
         (pdf-image-height-highlight (nth 1
                                          (car edges-of-match)))
         (pdf-height-fraction-to-scroll-to (/ (float pdf-image-height-highlight)
                                              (float pdf-image-height))))
    (pdf-util-tooltip-arrow pdf-height-fraction-to-scroll-to)))

(defun klin-is-this-link-to-be-opened-in-a-browser (link-str)
  (< 0 (length (split-string link-str "://"))))

(defun klin-open-same-buffer-in-new-frame ()
  (interactive)
  (let* ((cur-buf (current-buffer))
         (new-frame (make-frame-same-dim-as-cur-window))
         (new-window (car (window-list new-frame))))
    (with-selected-window new-window
      (find-file (buffer-file-name cur-buf)))))

(defun grep-find-in-notes-directories (&optional new-frame)
  (interactive)
  (when (file-exists-p my-org-notes-directory)
    (let* ((default-command-until-search-term (concat "find " my-org-notes-directory " -type f -name \"*.org\" -exec grep --color -nH --null -e "))
           (default-command-from-search-term " \{\} +")
           (new-frame)
           (command-string (read-shell-command "Run grep (like this): "
                                     (cons (concat default-command-until-search-term
                                                   default-command-from-search-term)
                                           (+ 1
                                              (string-bytes default-command-until-search-term))))))
      (unless new-frame
        (setq new-frame t))

      (when new-frame
        (setq new-frame (make-frame-same-dim-as-cur-window))
        (select-frame new-frame))

      (grep-find command-string))))

(defun org-get-headline-with-text-bounds ()
  (save-excursion
    (save-restriction
      (widen)
      (ignore-errors (outline-up-heading 1))
      (let* ((elt (org-element-at-point))
             (title (org-element-property :title elt))
             (beg (progn (org-end-of-meta-data t) (point)))
             (end (progn (outline-next-visible-heading 1) (point))))
        (list beg end)))))


(defun klin-populate-template (&optional source-path target-path open-dired-there)
  (interactive)
  (unless open-dired-there
    (setq open-dired-there t))
  (let* ((klin-template-dir "~/code_templates/")
         (default-command-until-source "cp -r ")
         (helm-ff-auto-update-initial-value nil))
    (when (file-exists-p klin-template-dir)
      (setq source-path (helm-read-file-name "Select a src file or dir: "
                                             :initial-input (if source-path source-path klin-template-dir)))
      (setq target-path (helm-read-file-name "Select and type target file or dir: "
                                             :initial-input (if target-path target-path (file-name-directory (buffer-file-name)))))
      (shell-command-to-string (read-string "Populate folder structure (like this): "
                                                   ;; (cons (concat default-command-until-source
                                                   ;;               ) (+ 1
                                        ; ;               (string-bytes default-command-until-search-term)))
                                                   (concat default-command-until-source
                                                           (prin1-to-string source-path)
                                                           " "
                                                           (prin1-to-string target-path))))))
  (when open-dired-there
    (dired target-path))
  target-path)

(defun klin-create-physics-note ()
  (interactive)
  (let* ((physics-notes-path "~/Dropbox/org/notes/grad/")
         (physics-notes-src-path (concat klin-template-dir "physics-notes"))
         (created-folder-path (klin-populate-template (when physics-notes-src-path physics-notes-src-path) physics-notes-path nil))
         (notes-file-path (concat (file-name-as-directory created-folder-path) "note.org")))
    (if (file-exists-p notes-file-path)
        (progn
          (find-file-existing notes-file-path)
          (goto-char (point-max)))
      (user-error (concat notes-file-path " does not exist, can't open it.")))))

(defun klin-populate-project-file-structure ()
  (interactive)
  (let* ((project-filestructure-path "~/Desktop")
         (project-filestructure-src-path (concat klin-template-dir ""))
         (created-folder-path (klin-populate-template (when project-filestructure-src-path project-filestructure-src-path) project-filestructure-path nil))
         ;; (notes-file-path (concat (file-name-as-directory created-folder-path) "note.org"))
         )
    ;; (if (file-exists-p notes-file-path)
    ;;     (progn
    ;;       (find-file-existing notes-file-path)
    ;;       (goto-char (point-max)))
    ;;   (user-error (concat notes-file-path " does not exist, can't open it.")))
    ))

(provide 'klin-optional)
;;; klin-optional.el ends here
