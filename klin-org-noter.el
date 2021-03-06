;;; klin-org-noter.el --- improvements to org-noter for my workflow  -*- lexical-binding: t; -*-

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

(require 'org)
(require 'org-noter)


(defun split-and-open-new-window (filepath-to-find)
  (interactive)
  (let* (new-window
         old-window-height (window-pixel-height))
    (setq new-window (split-window-below))
    (with-selected-window new-window
      (find-file filepath-to-find)
      (if (and (> (window-width nil 't) (* 0.9
                                           (frame-inner-width)))
               (> (frame-inner-width) (* 1.2
                                         (frame-inner-height))))
          (my-toggle-margins t)))
    (window-resize new-window (round (* -0.15 (window-size new-window nil t t)))nil nil t)
    new-window))

(defun cs-open-org-notes ()
  "Just open the notes file of org noter, without actually calling org-noter.
Sometimes it's nice to just write a few notes on the document (e.g. a paper)
and then directly link to the words (using my custom link),
without need for the full org-noter behaviour."
  (interactive)
  (let* ((buffname (buffer-file-name))
         (org-notes-path (klin-utils-pdf-get-org-notes-file-path (if (and (buffer-file-name)
                                                                          (string-equal "pdf"
                                                                                        (file-name-extension (buffer-file-name))))
                                                                     (buffer-file-name)
                                                                   (user-error "This file is not a pdf file!")))) existing-notes-window)
    (if (and org-notes-path
             (file-exists-p org-notes-path))
        (if (not (member org-notes-path (remove nil
                                                (mapcar (lambda (window)
                                                          (let* ((buffilename (buffer-file-name (window-buffer window))))
                                                            (when (and (char-or-string-p buffilename)
                                                                       (string-equal org-notes-path buffilename))
                                                              (setq existing-notes-window window))
                                                            buffilename))
                                                        (window-list)))))
            (split-and-open-new-window org-notes-path)
          (delete-window existing-notes-window))

      ;; create notes, like in org-noter
      (let* ((document-path (or buffer-file-name
                                buffer-file-truename
                                (error "This buffer does not seem to be visiting any file")))
             (document-name (file-name-nondirectory document-path))
             (document-base (file-name-base document-name))
             (search-names (reverse (list
                                     ;; the order gets flipped around in the selection dialog,
                                     ;; that's why I'm flipping it here, too (klin-utils-pdf-get-org-notes-file-path document-path)
                                     org-notes-path
                                     ;; (concat  document-base ".org")
                                     ;; (concat "." document-base ".org")
                                     )))
             (new-notes-path (completing-read "What name do you want the notes to have? "
                                              search-names     nil t)))
        (klin-utils-ask-to-create-dir (file-name-directory new-notes-path))

        ;; now write to the file and call this function recursively again to open the notes
        (let* (write-success-p)
          (with-temp-buffer
            (insert (concat "* Notes on "
                            (file-name-base buffname)))
            (insert "\n")
            (write-region (point-min)
                          (point-max)
                          org-notes-path))
          (if (file-exists-p org-notes-path)
              (split-and-open-new-window org-notes-path)
            (user-error (concat "File " org-notes-path " still doesn't exist!"))))))))


(setq org-noter-always-create-frame t)

(defun org-noter (&optional arg)
  "Start `org-noter' session.

There are two modes of operation. You may create the session from:
- The Org notes file
- The document to be annotated (PDF, EPUB, ...)

- Creating the session from notes file -----------------------
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

- Creating the session from the document ----------------------
This will try to find a notes file in any of the parent folders.
The names it will search for are defined in `org-noter-default-notes-file-names'.
It will also try to find a notes file with the same name as the
document, giving it the maximum priority.

When it doesn't find anything, it will interactively ask you what
you want it to do. The target notes file must be in a parent
folder (direct or otherwise) of the document.

You may pass a prefix ARG in order to make it let you choose the
notes file, even if it finds one."
  (interactive "P")
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

             ;; CHANGED:
             ;; (search-names (append org-noter-default-notes-file-names (list (concat document-base ".org"))))
             (search-names
               (append org-noter-default-notes-file-names
                      (reverse (list
                                ;; the order gets flipped around in the selection dialog,
                                ;; that's why I'm flipping it here, too
                                (klin-utils-pdf-get-org-notes-file-path document-path)
                                (concat document-base ".org")
                                (concat "." document-base ".org")
                                ))))
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
                                                     search-names  nil t))
                   list-of-possible-targets
                   target)

              ;; CHANGED: after completing read, ask to create the path if it's not already there
              (klin-utils-ask-to-create-dir (file-name-directory notes-file-name))

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
                        (setq file-name
                              (propertize file-name
                                          'display
                                          (concat file-name
                                                  (propertize " -- Exists!"
                                                              'face
                                                              '(foreground-color . "green"))))))
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
                 (throw 'break t))))))))))

  (my-after-org-noter-is-ready)
  ;; run this after some time, when the window has been opened rendered
  (run-with-idle-timer 0.05 nil (lambda ()
                                  (interactive)
                                  (org-noter-goto-org-document-and-widen-buffer
                                   (org-noter--get-notes-window))))

  (run-with-idle-timer 0.05 nil (lambda ()
                                  (interactive)
                                  (let* ((notes-window (org-noter--get-notes-window)))
                                    (when notes-window
                                      (select-window notes-window)))))
  )


;; override org-noter--doc-goto-location to afterwards go to the notes buffer, if one exists
(defun org-noter--doc-goto-location (location)
  "Go to location specified by LOCATION."
  (org-noter--with-valid-session
   (let ((window (org-noter--get-doc-window))
         (mode (org-noter--session-doc-mode session)))
     (with-selected-window window
       (cond
        ((run-hook-with-args-until-success 'org-noter--doc-goto-location-hook mode location))

        ((memq mode '(doc-view-mode pdf-view-mode))
         (if (eq mode 'doc-view-mode)
             (doc-view-goto-page (car location))
           (pdf-view-goto-page (car location))
           ;; NOTE(nox): This timer is needed because the tooltip may introduce a delay,
           ;; so syncing multiple pages was slow
           (when (>= org-noter-arrow-delay 0)
             (when org-noter--arrow-location (cancel-timer (aref org-noter--arrow-location 0)))
             (setq org-noter--arrow-location
                   (vector (run-with-idle-timer org-noter-arrow-delay nil 'org-noter--show-arrow)
                           window
                           (cdr location)))))
         (image-scroll-up (- (org-noter--conv-page-percentage-scroll (cdr location))
                             (window-vscroll))))

        ((eq mode 'nov-mode)
         (setq nov-documents-index (car location))
         (nov-render-document)
         (goto-char (cdr location))
         (recenter)))
       ;; NOTE(nox): This needs to be here, because it would be issued anyway after
       ;; everything and would run org-noter--nov-scroll-handler.
       (redisplay))))
  (let* ((notes-window (org-noter--get-notes-window)))
    (when notes-window
      (select-window notes-window))))

;; (require 'cs-latex-fragments-preview)

(defun my-wrap-lines-correctly ()
  (interactive)
  (let* ()
    (toggle-truncate-lines -1)
    (visual-line-mode 1)))

(defvar-local my-org-noter-buffer-lines-wrapped-correctly nil)

(defun my-org-noter-wrap-lines-correctly ()
  "Sometimes, after launching an org-noter session, lines weren't wrapped correctly. This fixes it."
  (interactive)
  (let* ((notes-window (org-noter--get-notes-window)))
    (if (and notes-window
             (not my-org-noter-buffer-lines-wrapped-correctly))
        (progn
          (select-window notes-window)
          (my-wrap-lines-correctly)
          (setq my-org-noter-buffer-lines-wrapped-correctly t)
          (message "Lines should be correctly wrapped now."))
      (message "No notes window yet available for wrapping lines correctly!"))))

(defun my-after-org-noter-is-ready ()
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min)
                            (point-max)
                            '(read-only t)))
  (let* ((org-noter-notes-window (org-noter--get-notes-window)))
    (if org-noter-notes-window
        (select-window org-noter-notes-window)))
  ;; latex now renders text-only subscripts properly
  (setq org-export-with-sub-superscripts nil)
  (message "Done."))

(defvar-local my-org-noter-buffer-widened nil)

(defun my-org-noter-widen-buffer ()
    "This removes the narrowing and enables seeing things above first heading.
Useful if you want to add e.g. some LATEX_HEADER."
    (interactive)
    (widen)
    ;; (message "the org-noter buffer has been widened")
    )

;; afterwards by default go to the org document and widen the buffer
(defun org-noter-goto-org-document-and-widen-buffer (&optional explicit-notes-window)
  (interactive)
  (let* ((notes-window (if explicit-notes-window
                           explicit-notes-window
                         (org-noter--get-notes-window))))
    (if notes-window
        (progn
          (with-selected-window notes-window
           ;;(select-window notes-window)
           (my-org-noter-widen-buffer))
          ;; (setq my-org-noter-buffer-widened t)
          ;; (message "Notes buffer has been widened")
          )
      (message "No notes window yet available for widening"))))


;; --- other highlighting function

(require 'highlight)

(defun cs-org-noter-pdf-notes-extents ()
  (let* ((begin (let* ((contents-begin (plist-get (car (cdr (org-noter--parse-root)))
                                                  :contents-begin))
                       headline-begin)
                  contents-begin
                  ;; (save-excursion
                  ;;   (when contents-begin
                  ;;     (forward-line -1)
                  ;;     (move-beginning-of-line nil)
                  ;;     (setq headline-begin (point))))
                  ))
         (end (plist-get (car (cdr (org-noter--parse-root)))
                                  :contents-end)))
    (list begin end)))

(defface my-face
  '((t
     ;; :background "#041424"
     ;; :background "#152330"
     ;; :background "#28323B"  ; doom citylights text selection color
     ;; :weight bold
     ;; :underline t
     ))
  "Face for highlighting active org-noter section."
  :group 'my-basic-faces)

(defun cs-highlight-org-noter-section ()
  (let* ((begin-end (cs-org-noter-pdf-notes-extents))
         (begin (nth 0 begin-end))
         (end (nth 1 begin-end)))
    ;; face: e.g. azure (choose optionally using hlg-choose-default face), MSG can be nil
    (hlt-choose-default-face 'my-face)
    (hlt-highlight-lines begin end nil nil)
    ))

(defun org-noter-jump-into-it-next-note ()
  "In an org-buffer (not org-noter), search the next NOTER_PAGE or NOTER_DOCUMENT property.
Then launch org-noter in there."
  (interactive)
  (let* ((point-before (point))
         (point-page (save-excursion
                       (re-search-forward (regexp-quote ":NOTER_PAGE:")
                                          nil
                                          t)
                       (when (not (eq (point) point-before))
                         (point))))
         (point-doc (save-excursion
                      (re-search-forward (regexp-quote ":NOTER_DOCUMENT:")
                                         nil
                                         t)
                      (when (not (eq (point) point-before))
                        (point)))))
    (cond
     ((and point-page
           (not point-doc))
      (goto-char point-page))
     ((and point-doc
           (not point-page))
      (goto-char point-doc))
     ((and point-doc point-page)
      (if (< (abs (- (point)
                     point-doc)) (abs (- (point)
                     point-page)))
          (goto-char point-doc)
        (goto-char point-page)))))
  (org-noter)
  (org-noter-goto-org-document-and-widen-buffer))

(defun org-noter-jump-into-it-prev-note ()
  "In an org-buffer (not org-noter), search the prev NOTER_PAGE or NOTER_DOCUMENT property.
Then launch org-noter in there."
  (interactive)
  (let* ((point-before (point))
         (point-page (save-excursion
                       (re-search-backward (regexp-quote ":NOTER_PAGE:")
                                           nil
                                           t)
                       (when (not (eq (point) point-before))
                         (point))))
         (point-doc (save-excursion
                      (re-search-backward (regexp-quote ":NOTER_DOCUMENT:")
                                          nil
                                          t)
                      (when (not (eq (point) point-before))
                        (point)))))
    (cond
     ((and point-page
           (not point-doc))
      (goto-char point-page))
     ((and point-doc
           (not point-page))
      (goto-char point-doc))
     ((and point-doc point-page)
      (if (< (abs (- (point)
                     point-doc)) (abs (- (point)
                     point-page)))
          (goto-char point-doc)
        (goto-char point-page)))))
  (org-noter)
  (org-noter-goto-org-document-and-widen-buffer))

;; copy org-noter-sync-previous-note and make an adapted function that is mapped to a different key
;; that switches up into the really wanted pdf
;; Simply override this one to stay in the notes buffer

(defun org-noter-sync-prev-note ()
  "Go to the location of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         (current-begin (org-element-property :begin (org-noter--get-containing-heading)))
         previous)
     (when current-begin
       (org-noter--map-ignore-headings-with-doc-file
        contents t
        (when location
          (if (= current-begin (org-element-property :begin headline))
              t
            (setq previous headline)
            nil))))

     (if previous
         (progn
           ;; NOTE(nox): This needs to be manual so we can focus the correct note
           (org-noter--doc-goto-location (org-noter--parse-location-property previous))
           (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session previous)))
       (user-error "There is no previous note"))))
  (org-noter-goto-org-document-and-widen-buffer))

(defun org-noter-sync-current-note ()
  "Go the location of the selected note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (if (string= (org-entry-get nil org-noter-property-doc-file t) (org-noter--session-property-text session))
       (let ((location (org-noter--parse-location-property (org-noter--get-containing-heading))))
         (if location
             (org-noter--doc-goto-location location)
           (user-error "No note selected")))
     (user-error "You are inside a different document")))
    (org-noter-goto-org-document-and-widen-buffer))

(defun org-noter-sync-next-note ()
  "Go to the location of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         next)

     (org-noter--map-ignore-headings-with-doc-file
      contents t
      (when (and location (< (point) (org-element-property :begin headline)))
        (setq next headline)))

     (if next
         (progn
           (org-noter--doc-goto-location (org-noter--parse-location-property next))
           (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session next)))
       (user-error "There is no next note"))))
    (org-noter-goto-org-document-and-widen-buffer))

(defun org-noter-sync-prev-note-in-prev-pdf ()
  "Call this from within the notes window. Go to the location of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         (current-begin (org-element-property :begin (org-noter--get-containing-heading)))
         previous)
     (when current-begin
       (org-noter--map-ignore-headings-with-doc-file
        contents t
        (when location
          (if (= current-begin (org-element-property :begin headline))
              t
            (setq previous headline)
            nil))))

     (if previous
         (progn
           ;; NOTE(nox): This needs to be manual so we can focus the correct note
           (org-noter--doc-goto-location (org-noter--parse-location-property previous))
           (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session previous)))
       (message "There is no previous note for this document")
       ;; CHANGED: now go and search for a previous heading with org-notes in it
       (select-window (org-noter--get-notes-window))
       (org-noter-goto-org-document-and-widen-buffer)
       (let* ((cur-pos (point))
              (doc-root-begin (plist-get (car (cdr (org-noter--parse-root)))
                                         :contents-begin))
              prev-noter-doc-pos
              prev-noter-page-pos
              jump-to-pos)
         (if (save-excursion
               (goto-char doc-root-begin)
               (if (setq prev-noter-doc-pos (re-search-backward ":NOTER_DOCUMENT:" nil 'continue))
                   (progn
                     (setq jump-to-pos prev-noter-doc-pos)
                     (if (setq prev-noter-page-pos (re-search-forward ":NOTER_PAGE:" nil 'continue))
                         (setq jump-to-pos prev-noter-page-pos))
                     jump-to-pos)))
             (progn
               (goto-char jump-to-pos)
               (org-noter))
           (user-error "There is no previous note, not even a previous document")))
       )))
  (org-noter-goto-org-document-and-widen-buffer))

(defun org-noter-sync-next-note-in-next-pdf ()
  "Go to the location of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         next)

     (org-noter--map-ignore-headings-with-doc-file
      contents t
      (when (and location (< (point) (org-element-property :begin headline)))
        (setq next headline)))

     (if next
         (progn
           (org-noter--doc-goto-location (org-noter--parse-location-property next))
           (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session next)))
       (message "There is no next note for this document")
       ;; CHANGED: now go and search for a next heading with org-notes in it
       (select-window (org-noter--get-notes-window))
       (org-noter-goto-org-document-and-widen-buffer)
       (let* ((cur-pos (point))
              (doc-root-end (plist-get (car (cdr (org-noter--parse-root)))
                                       :contents-end))
              next-noter-doc-pos
              next-noter-page-pos
              jump-to-pos)
         (if (setq jump-to-pos
                   (save-excursion
                     (goto-char doc-root-end)
                     (if (setq next-noter-doc-pos (re-search-forward ":NOTER_DOCUMENT:" nil 'continue))
                         (progn
                           (setq jump-to-pos next-noter-doc-pos)
                           (if (setq next-noter-page-pos
                                     (re-search-forward ":NOTER_PAGE:" nil 'continue))
                               (setq jump-to-pos next-noter-page-pos))))
                     jump-to-pos))
             (progn
               (let* (notes-window-then
                      (ws (window-start)))
                 (if (setq notes-window-then (org-noter--get-notes-window))
                     (progn
                       (org-noter-goto-org-document-and-widen-buffer)
                       (set-window-start (selected-window) ws)
                       (goto-char jump-to-pos))))
               (goto-char jump-to-pos)
               (org-noter))
           (user-error "There is no next note, not even a next document")))
       )))
  (org-noter-goto-org-document-and-widen-buffer))

(defun org-noter-switch-to-base-buffer (
                                        ;;&optional doc-too
                                        )
  "Switch to base buffer, but keep point and background scroll position."
  (interactive)
  (let* ((notes-window (org-noter--get-notes-window))
         (doc-window (org-noter--get-doc-window)))
    (let* ((ws (window-start)) notes-window-then
           point-pos-then
           notes-buffer
           notes-buffer-base-buffer
           )
      (if (org-noter--get-notes-window)
          (progn
            (org-noter--with-selected-notes-window
             (setq point-pos-then (point))
             (setq notes-buffer-base-buffer (buffer-base-buffer))
             )
            (select-window (org-noter--get-notes-window))
            (switch-to-buffer notes-buffer-base-buffer nil t)
            ;; (set-window-start (selected-window) ws)
            (goto-char point-pos-then)
            ;; unfold all headings to the way of the cursor
            (org-show-entry)
            (message "In base-buffer now"))
        (user-error "Not in org-noter session")))
    ;; (when doc-too)
    (when t
      (let* ()
        ;; (select-window (org-noter--get-notes-window))
        ;; go to the doc window, too
        (with-selected-window doc-window
          (when (string-equal (file-name-extension (buffer-file-name))
                              "pdf")
            (let* ((bmk-record (pdf-view-bookmark-make-record)))
              (undedicate-window-and-switch-to-base-buffer)
              (pdf-view-bookmark-jump bmk-record))))))))

;; (add-hook 'org-noter-doc-mode-hook
;;           'org-noter-goto-org-document-and-widen-buffer t)


(defun org-noter-switch-to-base-buffer (
                                        ;;&optional doc-too
                                        )
  "Switch to base buffer, but keep point and background scroll position."
  (interactive)
  (let* ((notes-window (org-noter--get-notes-window))
         (doc-window (org-noter--get-doc-window)))
    (let* ((ws (window-start)) notes-window-then
           point-pos-then
           notes-buffer
           notes-buffer-base-buffer
           )
      (if (org-noter--get-notes-window)
          (progn
            (org-noter--with-selected-notes-window
             (setq point-pos-then (point))
             (setq notes-buffer-base-buffer (buffer-base-buffer))
             )
            (select-window (org-noter--get-notes-window))
            (switch-to-buffer notes-buffer-base-buffer nil t)
            ;; (set-window-start (selected-window) ws)
            (goto-char point-pos-then)
            ;; unfold all headings to the way of the cursor
            (org-show-entry)
            (message "In base-buffer now"))
        (user-error "Not in org-noter session")))
    ;; (when doc-too)
    (when t
      (let* ()
        ;; (select-window (org-noter--get-notes-window))
        ;; go to the doc window, too
        (with-selected-window doc-window
          (when (string-equal (file-name-extension (buffer-file-name))
                              "pdf")
            (let* ((bmk-record (pdf-view-bookmark-make-record)))
              (undedicate-window-and-switch-to-base-buffer)
              (pdf-view-bookmark-jump bmk-record))))))))


(add-hook 'org-noter-notes-mode-hook
          (lambda ()
            (setq line-spacing 0.25)
            (redraw-frame (selected-frame))))

(defun undedicate-window-and-switch-to-base-buffer ()
  "For a pdf, restore the scroll state of the clone's buffer."
  (interactive)
  (let* ()
    (set-window-dedicated-p (frame-selected-window)
                            nil)
    (switch-to-buffer (buffer-base-buffer)
                      nil
                      t)
    ))

(defun org-noter-insert-pdf-headings (&optional pdf-file-paths)
  (interactive)
  (let* ((ctr 0) make-them-top-level
         use-relative-file-paths)
    (unless pdf-file-paths
      (setq pdf-file-paths (helm-read-file-name "Add org-noter headlines for these PDFs:"
                                                :initial-input (if (buffer-file-name)
                                                                   (file-name-directory (buffer-file-name))
                                                                 (buffer-base-buffer (buffer-file-name))):marked-candidates
                                                t)))
    ;; check if they are all pdfs
    (unless (eq (length (remove nil
                                (mapcar (lambda (filepath)
                                          (when (string-equal (file-name-extension filepath)
                                                              "pdf")
                                            filepath))
                                        pdf-file-paths))) (length pdf-file-paths))
      (user-error "They aren't all pdfs, please make sure that all are PDFs"))
    (if (yes-or-no-p "Should they be top-level-headings?")
        (setq make-them-top-level t))
    (if (yes-or-no-p "Use relative filepaths?")
        (setq use-relative-file-paths t))
    (while (< ctr (length pdf-file-paths))
      ;; jump to the end of the current subtree (to insert the headings there)
      (org-end-of-subtree)
      (if make-them-top-level
          (org-insert-heading nil nil t)
        (org-insert-heading))
      (insert (file-name-base (nth ctr pdf-file-paths)))
      (org-insert-property-drawer)
      (org-set-property "NOTER_DOCUMENT"
                        (if use-relative-file-paths
                            (file-relative-name (nth ctr pdf-file-paths))
                          (klin-utils-get-reduced-file-path (nth ctr pdf-file-paths))))
      (setq ctr (+ ctr 1)))))

(provide 'klin-org-noter)
;;; klin-org-noter.el ends here
