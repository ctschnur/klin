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


;; ---------- org-mode getting and setting global document properties
;; thanks to Tobias' answer at https://emacs.stackexchange.com/a/21472

(require 'cl-lib)

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

(defun klin-org-noter-override-org-noter (&optional arg)
    "Start `org-noter' session.

There are two modes of operation. You may create the session from:
- The Org notes file
- The document to be annotated (PDF, EPUB, ...)

- Creating the session from notes file -----------------------------------------
This will open a session for taking your notes, with indirect
buffers to the document and the notes side by side. Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this document). If no document path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With 2 prefix universal arguments ARG, ask for a new document,
even if the current heading annotates one.

With a prefix number ARG:
- Greater than 0: Open the document like `find-file'
-     Equal to 0: Create session with `org-noter-always-create-frame' toggled
-    Less than 0: Open the folder containing the document

- Creating the session from the document ---------------------------------------
pThis will try to find a notes file in any of the parent folders.
The names it will search for are defined in `org-noter-default-notes-file-names'.
It will also try to find a notes file with the same name as the
document, giving it the maximum priority.

When it doesn't find anything, it will interactively ask you what
you want it to do. The target notes file must be in a parent
folder (direct or otherwise) of the document.

You may pass a prefix ARG in order to make it let you choose the
notes file, even if it finds one."
    (interactive "P")
;;     (let ((minibuffer-message-timeout 0))
;;       (message "%s" "WARNING: Running custom ADVICE (override) \"
;; \"for (org-noter): declared in /Dropbox/stuff/klin/klin-optional.el"))
  (cond
   ;; NOTE(nox): Creating the session from notes file
   ((eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (error "`org-noter' must be issued inside a heading"))

    (let* ((notes-file-path (buffer-file-name))
           (document-property (org-noter--get-or-read-document-property (not (equal arg '(4)))
                                                                        (equal arg '(16))))
           (org-noter-always-create-frame
            (if (and (numberp arg) (= arg 0)) (not org-noter-always-create-frame) org-noter-always-create-frame))
           (ast (org-noter--parse-root (vector (current-buffer) document-property))))

      (when (catch 'should-continue
              (when (or (numberp arg) (eq arg '-))
                (cond ((> (prefix-numeric-value arg) 0)
                       (find-file document-property)
                       (throw 'should-continue nil))
                      ((< (prefix-numeric-value arg) 0)
                       (find-file (file-name-directory document-property))
                       (throw 'should-continue nil))))

              ;; NOTE(nox): Check if it is an existing session
              (let ((id (get-text-property (org-element-property :begin ast) org-noter--id-text-property))
                    session)
                (when id
                  (setq session (cl-loop for test-session in org-noter--sessions
                                         when (= (org-noter--session-id test-session) id)
                                         return test-session))
                  (when session
                    (let* ((org-noter--session session)
                           (location (org-noter--parse-location-property (org-noter--get-containing-heading))))
                      (org-noter--setup-windows session)
                      (when location (org-noter--doc-goto-location location))
                      (select-frame-set-input-focus (org-noter--session-frame session)))
                    (throw 'should-continue nil))))
              t)
        (org-noter--create-session ast document-property notes-file-path))))

   ;; NOTE(nox): Creating the session from the annotated document
   ((memq major-mode '(doc-view-mode pdf-view-mode nov-mode))
    (if (org-noter--valid-session org-noter--session)
        (progn (org-noter--setup-windows org-noter--session)
               (select-frame-set-input-focus (org-noter--session-frame org-noter--session)))

      ;; NOTE(nox): `buffer-file-truename' is a workaround for modes that delete
      ;; `buffer-file-name', and may not have the same results
      (let* ((buffer-file-name (or buffer-file-name (bound-and-true-p nov-file-name)))
             (document-path (or buffer-file-name buffer-file-truename
                                (error "This buffer does not seem to be visiting any file")))
             (document-name (file-name-nondirectory document-path))
             (document-base (file-name-base document-name))
             (document-directory (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   (if (file-equal-p document-name buffer-file-truename)
                                       default-directory
                                     (file-name-directory buffer-file-truename))))
             ;; NOTE(nox): This is the path that is actually going to be used, and should
             ;; be the same as `buffer-file-name', but is needed for the truename workaround
             (document-used-path (expand-file-name document-name document-directory))

             ;; CHANGED: THIS LINE IS THE ONLY THING I CHANGED
             ;; (search-names (append org-noter-default-notes-file-names (list (concat document-base ".org"))))
             (search-names (append org-noter-default-notes-file-names (list (concat "." document-name ".org"))))
             notes-files-annotating     ; List of files annotating document
             notes-files                ; List of found notes files (annotating or not)

             (document-location (org-noter--doc-approx-location)))

        ;; NOTE(nox): Check the search path
        (dolist (path org-noter-notes-search-path)
          (dolist (name search-names)
            (let ((file-name (expand-file-name name path)))
              (when (file-exists-p file-name)
                (push file-name notes-files)
                (when (org-noter--check-if-document-is-annotated-on-file document-path file-name)
                  (push file-name notes-files-annotating))))))

        ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
        ;; and it will end up in the correct order
        (dolist (name search-names)
          (let ((directory (locate-dominating-file document-directory name))
                file)
            (when directory
              (setq file (expand-file-name name directory))
              (unless (member file notes-files) (push file notes-files))
              (when (org-noter--check-if-document-is-annotated-on-file document-path file)
                (push file notes-files-annotating)))))

        (setq search-names (nreverse search-names))

        (when (or arg (not notes-files-annotating))
          (when (or arg (not notes-files))
            (let* ((notes-file-name (completing-read "What name do you want the notes to have? "
                                                     search-names nil t))
                   list-of-possible-targets
                   target)

              ;; NOTE(nox): Create list of targets from current path
              (catch 'break
                (let ((current-directory document-directory)
                      file-name)
                  (while t
                    (setq file-name (expand-file-name notes-file-name current-directory))
                    (when (file-exists-p file-name)
                      (setq file-name (propertize file-name 'display
                                                  (concat file-name
                                                          (propertize " -- Exists!"
                                                                      'face '(foreground-color . "green")))))
                      (push file-name list-of-possible-targets)
                      (throw 'break nil))

                    (push file-name list-of-possible-targets)

                    (when (string= current-directory
                                   (setq current-directory
                                         (file-name-directory (directory-file-name current-directory))))
                      (throw 'break nil)))))
              (setq list-of-possible-targets (nreverse list-of-possible-targets))

              ;; NOTE(nox): Create list of targets from search path
              (dolist (path org-noter-notes-search-path)
                (when (file-exists-p path)
                  (let ((file-name (expand-file-name notes-file-name path)))
                    (unless (member file-name list-of-possible-targets)
                      (when (file-exists-p file-name)
                        (setq file-name (propertize file-name 'display
                                                    (concat file-name
                                                            (propertize " -- Exists!"
                                                                        'face '(foreground-color . "green"))))))
                      (push file-name list-of-possible-targets)))))

              (setq target (completing-read "Where do you want to save it? " list-of-possible-targets
                                            nil t))
              (set-text-properties 0 (length target) nil target)
              (unless (file-exists-p target) (write-region "" nil target))

              (setq notes-files (list target))))

          (when (> (length notes-files) 1)
            (setq notes-files (list (completing-read "In which notes file should we create the heading? "
                                                     notes-files nil t))))

          (if (member (car notes-files) notes-files-annotating)
              ;; NOTE(nox): This is needed in order to override with the arg
              (setq notes-files-annotating notes-files)
            (with-current-buffer (find-file-noselect (car notes-files))
              (goto-char (point-max))
              (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                      "* " document-base)
              (org-entry-put nil org-noter-property-doc-file
                             (file-relative-name document-used-path
                                                 (file-name-directory (car notes-files)))))
            (setq notes-files-annotating notes-files)))

        (when (> (length (cl-delete-duplicates notes-files-annotating :test 'equal)) 1)
          (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                              notes-files-annotating nil t))))

        (with-current-buffer (find-file-noselect (car notes-files-annotating))
          (org-with-wide-buffer
           (catch 'break
             (goto-char (point-min))
             (while (re-search-forward (org-re-property org-noter-property-doc-file) nil t)
               (when (file-equal-p (expand-file-name (match-string 3)
                                                     (file-name-directory (car notes-files-annotating)))
                                   document-path)
                 (let ((org-noter--start-location-override document-location))
                   (org-noter))
                 (throw 'break t)))))))))))

(add-function :override (symbol-function 'org-noter) #'klin-org-noter-override-org-noter)

;; (defun klin-optional--override-org-noter (arg)
;;   "Add the suggestion .example.pdf.org to org-noter-default-notes-file-names.
;; If org-noter is started from within a pdf view (my workflow usually),
;; help it find my standard associated pdf notes org file (my convention),
;; within the same directory.
;; Strategy:
;; - you are in the pdf; open the assoc org notes file with find-file
;; - then, run (org-noter) on it, enabling org-noter
;; - afterwards, the program may run org-noter again, but no worries:
;;   if it's already running, running it a second time doesn't do anything"
;;   ;; first of all, remove the advice immediateley, because we want
;;   ;; to actually call (org-noter) unadvised from in here
;;   (remove-function (symbol-function 'org-noter)
;;                    #'klin-optional--override-org-noter)
;;   (let* ((orig-buffer (current-buffer))
;;          bmk-original-buffer
;;          bmk-record-for-restore
;;          orig-pdf-file-path
;;          orig-pdf-file-name
;;          conv-org-file-name
;;          directory-path
;;          conv-org-file-path
;;          new-org-file-buffer)
;;     (if (eq major-mode 'pdf-view-mode)
;;         (progn
;;           (setq orig-pdf-file-path (buffer-file-name))
;;           (setq orig-pdf-file-name
;;                 (file-name-nondirectory orig-pdf-file-path))

;;           (setq bmk-original-buffer (pdf-view-bookmark-make-record))

;;           (setq directory-path (file-name-directory orig-pdf-file-path))
;;           (setq conv-org-file-name
;;                 (concat "." (file-name-nondirectory orig-pdf-file-path) ".org"))
;;           ;; ask if my convention org-file exists
;;           (setq conv-org-file-path (concat directory-path conv-org-file-name))
;;           (if (file-exists-p conv-org-file-path)
;;               (progn
;;                 (find-file-existing conv-org-file-path)
;;                 (org-noter)
;;                 (current-buffer)
;;                 (selected-window)
;;                 (pdf-view-bookmark-jump
;;                  (append (list (buffer-name (window-buffer (selected-window))))
;;                          (cdr bmk-original-buffer))))
;;             (helm-find-files-1 conv-org-file-name)
;;             (org-insert-heading)
;;             (insert "Notes")
;;             (org-set-property "NOTER_DOCUMENT" orig-pdf-file-name)
;;             (setq new-org-file-buffer (current-buffer))
;;             ;; (org-set-property "NOTER_PAGE" "1")
;;             (save-buffer)
;;             (org-noter)
;;             (pdf-view-bookmark-jump
;;                  (append (list (buffer-name (window-buffer (selected-window))))
;;                          (cdr bmk-original-buffer)))
;;             )))

;;     (add-function :override (symbol-function 'org-noter) #'klin-optional--override-org-noter)))

;; ;; at the beginning, advise the function
;; (add-function :override (symbol-function 'org-noter) #'klin-optional--override-org-noter)


;; ------- toying around with buttons

;; (defun wh/help-hello-world ()
;;   (interactive)
;;   (with-help-window (help-buffer)
;;     (princ "foo_bar is a function.\n\nIt does stuff.")
;;     (shrink-window 1)))

;; (defun button-pressed (button)
;;   (message (format "Button pressed!")))

;; (define-button-type 'custom-button
;;   'action 'button-pressed
;;   'follow-link t
;;   'help-echo "Click Button"
;;   'help-args "test"

;;   (make-button 1 10 :type 'custom-button))

;; --------


;; --------- KEY DEFININITIONS
;; --------- directly embed pictures or e.g. handwritten notes from cloud
(define-key org-mode-map (kbd "C-M-, w") 'klin-org-watch-and-insert-scanned-file)

;; --------- some key bindings for pdf-view-mode to make it more chrome-like
(define-key pdf-view-mode-map (kbd "<S-mouse-5>") 'image-forward-hscroll)
(define-key pdf-view-mode-map (kbd "<S-mouse-4>") 'image-backward-hscroll)

(define-key pdf-view-mode-map (kbd "<C-mouse-5>") (lambda () (interactive) (pdf-view-enlarge 1.1)))
(define-key pdf-view-mode-map (kbd "<C-mouse-4>") (lambda () (interactive) (pdf-view-shrink 1.1)))
;; ---------


(provide 'klin-optional)
;;; klin-optional.el ends here
