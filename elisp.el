(require 'frame-cmds)

;; TODO:
;; I am in an org file and want to cite something, that is not in crossref
;; It needs a bibtex entry, to create that, I call
;; (org-ref-bibtex-new-entry/bibtex-Book-and-exit)

;; some useful stuff i figured out how to do:

;; get the entry's text from the entry's key
;; (org-ref-get-bibtex-entry "elb05")

;; get the full file name of the book from search
;; (nth 0 (split-string (shell-command-to-string "find ~/Dropbox -name \"*elberfel*\"") "\n"))

;; (let* ((results )
;;            (bibfile (cdr results))
;;            entry))

;; (car (org-ref-get-bibtex-key-and-file key))
;;
;; (let* ((results (org-ref-get-bibtex-key-and-file key))
;;        (bibfile (cdr results))
;;        entry))
;;
;; (let* ((results (org-ref-get-bibtex-key-and-file key))
;;        (bibfile (cdr results))
;;        entry))
;;
;;     (with-temp-buffer
;;       (insert-file-contents file)
;;       (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
;;       (bibtex-search-entry key nil 0)
;;       (setq bibtex-entry (bibtex-parse-entry))
;;       ;; downcase field names so they work in the format-citation code
;;       (dolist (cons-cell bibtex-entry)
;;         (setf (car cons-cell) (downcase (car cons-cell))))
;;       (setq entry-type (downcase (cdr (assoc "=type=" bibtex-entry))))
;;       (setq format (cdr (assoc entry-type org-ref-bibliography-entry-format)))
;;       (if format
;;           (setq entry  (org-ref-reftex-format-citation bibtex-entry format))
;;         ;; if no format, we use the bibtex entry itself as a fallback
;;         (save-restriction
;;           (bibtex-narrow-to-entry)
;;           (setq entry (buffer-string)))))

;; (org-ref-get-bibtex-entry "elb05")
;;
;; (setq key "elb05")
;; (setq results (org-ref-get-bibtex-key-and-file key))
;; (setq bibfile (cdr results))

;; --- other stuff (testing) -----
;; (setq file-page-offset (string-to-number (klin-bibtex-get-field "file-page-offset" nil "elb05")))
;; (setq pdf-filepath (klin-bibtex-get-field "filepath" "elb05"))
;; great, this works

;; (defun open-bibtex-pdf-at-point (cite-str)
;;   (setq strparts (split-string cite-str ":"))
;;   (setq key (nth 1 strparts))
;;   (setq page (string-to-number (nth 2 strparts)))
;;   (setq file-page-offset (string-to-number (klin-bibtex-get-field "file-page-offset" key)))
;;   (setq pdf-filepath (klin-bibtex-get-field "filepath" key))
;;   (progn
;;     (find-file-other-frame pdf-filepath)
;;     (pdf-view-goto-page (- (+ page file-page-offset) 1))))

;; (open-bibtex-pdf-at-point "cite:elb05:30")

;; (defun read-point-string-and-trigger-opening ()
;;   (setq cite-str (concat "cite:" (org-ref-get-bibtex-key-under-cursor)))
;;   (open-bibtex-pdf-at-point cite-str))

;; (define-key org-mode-map (kbd "C-c o") 'read-point-string-and-trigger-opening)

;; --- even more stuff (testing) ----
;; (progn
;;   (string-match "p\\.\\s-*\\([0-9]*\\)" description)
;;   (setq page-str (match-string 1 description))
;;   (open-bibtex-document-on-page bibtexkey (string-to-number page-str)))


;; (defun write-desktop-file ()
;;   (if (make-directory (concat projectile-project-root "emacs-desktop") 'parents)
;;       (desktop-save desktop-dirname)
;;   )

;; (defun open-pdf-in-new-frame-if-not-already-open ()
;;   ;; useful functions
;;   ;; (find-file-existing (setq filename (expand-file-name "~/Dropbox/2TextBooks/1-Bible/elberfelder-1905-deuelo_a4.pdf")))
;;   ;; (iconify-frame (nth 0 (frame-list)))
;;   ;; (buffer-list)
;;   ;; (visible-frame-list)
;;
;;   (setq buffer (get-buffer "elberfelder-1905-deuelo_a4.pdf"))
;;   (setq buffer-window (get-buffer-window buffer 0))
;;
;;   (if buffer
;;       (if buffer-window
;;           (progn
;;             (setq framewithpdf (window-frame buffer-window))
;;             (if (frame-visible-p framewithpdf)
;;                 (raise-frame framewithpdf))
;;             (make-frame-visible framewithpdf)
;;             (raise-frame framewithpdf))
;;         (switch-to-buffer-other-frame))
;;     (find-file-other-frame (setq filename (expand-file-name "~/Dropbox/2TextBooks/1-Bible/elberfelder-1905-deuelo_a4.pdf"))))
;;   )

;; TODO: because un-iconify for some reason doesn't work
;; in gnome through and Emacs 25, 26 (at least through raise-frame)
;; i do it only with visible and invisible frames, (which btw. don't show up in
;; gnome's window switcher.)

(defun klin-bibtex-get-field (field &optional key bibfile-path)
  "Returns the value of the key value pair (FIELD, value) of a bibtex entry.
   When called from an org-mode buffer with org-ref installed,
   it only needs the bibtex KEY if an org-ref -style bibliography link
   in the org buffer links to a collective .bib file.
   Otherwise, it needs an explicit bibfile-path.
   If it's just a single entry, the KEY is optional."

  (unless key (setq key "negele98_quant"))
  (unless bibfile-path (setq bibfile-path (cdr (org-ref-get-bibtex-key-and-file key))))
  (unless bibfile-path (setq bibfile-path (expand-file-name "~/Dropbox/2TextBooks/.1-NegeleOrland-QuantumManyParticeSystems.pdf.bib")))

  (let* (entry)
    (with-temp-buffer
      (bibtex-mode)
      (insert-file-contents bibfile-path)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      ;; refresh the parsing of the keys
      (bibtex-parse-keys)
      ;; if just one, get it's key
      (unless key
        (if (= 1 (length (bibtex-global-key-alist)))
            (setq key (car (nth 0 (bibtex-global-key-alist)))))
        )
      (unless key (setq key "negele98_quanti")) ;; debugging

      (if (not (bibtex-search-entry key nil 0)) ;; find the one you are looking for
          (progn
            (message (concat "no key " key " found in " bibfile-path)) nil)
        (setq entry (bibtex-parse-entry)) ;; sets cursor at the end of entry's last field
        (let ((field-value (org-ref-reftex-get-bib-field field entry)))
          (if (>= (length field-value) 1)
              (progn
                ;; (replace-regexp-in-string "/+" "/" field-value) ;; clean it up (?)
                field-value
                )
            (message "no field " field " found in " bibfile-path " -> " key) nil))
          ) ;; sets cursor at the beginning of the entry's line
      )))


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

(defun open-pdf-document-new-frame (&optional filepath page)
  "open a pdf file in a new frame"
  (unless page (setq page 1))
  (unless filepath (setq filepath (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  (progn
    (find-file-other-frame filepath)
    (pdf-view-goto-page page)))

(defun open-bibtex-document-on-page (bibtexkey page)
  (let* ((file-page-offset (string-to-number (klin-bibtex-get-field "file-page-offset" bibtexkey)))
         (filepath (klin-bibtex-get-field "filepath" bibtexkey))
         (page (- (+ page file-page-offset) 1)))
    (open-pdf-document-new-frame filepath page)
    )
  )

(defun openlink (mylist)
  (setq linktyp (nth 0 mylist))
  (setq bibtexkey (nth 1 mylist))
  (setq description (nth 2 mylist))
  (string-match "p\\.\\s-*\\([0-9]*\\)" description)
  (setq page-str (match-string 1 description))
  (open-bibtex-document-on-page bibtexkey (string-to-number page-str))
  )

(defun search-nearest-link-and-open ()
  (interactive)
  (openlink (get-link-info-nearest-to-point))
  )

(defun make-invisible ()
  (interactive)
  (make-frame-invisible (window-frame (get-buffer-window (current-buffer) t)))
  )

(defun make-visible (&optional bufname)
  (interactive)
  (unless bufname (setq bufname "elberfelder-1905-deuelo_a4.pdf"))
  (setq buffer (get-buffer bufname))
  (setq bufwindow (get-buffer-window buffer t))
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
    )
  )

;; make a helm selection list of all open buffers, if it's a pdf buffer you select, you open it in it's window's frame.

(defun get-all-pdf-buffers ()
  (interactive)
  (setq pdfbuffers (make-list 0 0))
  (setq i 0)
  (while (< i (length (buffer-list)))
    (setq bufname (buffer-name (nth i (buffer-list))))
    (if (and (string-match-p (regexp-quote ".pdf") bufname)
             (not (string-match-p (regexp-quote "\*") bufname)))
        (setq pdfbuffers (append pdfbuffers `(,bufname))))
    (setq i (+ i 1))
    )
  pdfbuffers)

(setq some-helm-source
      '((name . "make visible and bring to front PDF buffer(s)")
        (candidates . get-all-pdf-buffers)
        (action . (lambda (candidate)
                    (make-visible candidate)
                    ;; (message-box "%s" candidate)
                    ))))

(defun helm-browse-pdf-buffers ()
  (interactive)
  (helm :sources '(some-helm-source))
  )


(defun override-org-pdfview-store ()
  (interactive)
  (eval-after-load "org-pdfview" ;; package org-pdfview with org-pdfview.el
    (defun org-pdfview-store-link ()  ;; override a function there
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

(defun make-bibtex-file-for-pdf (&optional pdfpath isbn doi)
  (interactive)
  (unless pdfpath (setq pdfpath (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  (unless isbn (setq isbn "0-7382-0052-2"))
  (let* ((basedir (file-name-directory pdfpath))
         (filename (file-name-nondirectory pdfpath))
         (bibtexfilename (concat "." filename ".bib")) ;; "hidden" file
         (bibfilepath (concat basedir bibtexfilename))
         )
    (unless (file-exists-p pdfpath)
      (unless (yes-or-no-p "pdf file doesn't exist, continue anyway?")
        (error "pdf file doesn't exist, chose to quit")
        ))
    ;; (if (file-exists-p bibfilepath)
    ;;     (unless (yes-or-no-p (concat "bib file already exists at " bibfilepath ". Would you like a side by side view to edit it?"))
    ;;       )
    ;;   (if isbn
    ;;       (isbn-to-bibtex isbn nil)
    ;;     )
    ;;   )

    ;; create a bib file if it's not already there
    ;; (with-temp-buffer (write-file bibfilepath)) ;; this actually clears the content
    ;; (with-temp-buffer (write-file (expand-file-name "~/Dropbox/stuff/1Book/testfile.txt")))

    ;; this appends to a file (and creates one if there is none)
    (unless bibfilepath (setq bibfilepath (expand-file-name "~/Dropbox/stuff/1Book/testfile.txt")))
    (with-temp-buffer (write-region "" nil bibfilepath 'append))
    (side-by-side-bibtex-edit bibfilepath)
    )
  )

(defun side-by-side-bibtex-edit (&optional existing-bibfile-path alternative-bibtex-entry-str)
  "bib file already exists somewhere, don't overwrite it, complete it with another
   alternative bibtex entry delivered to the function"
  (interactive)
  ;; (unless existing-bibfile-path (setq existing-bibfile-path (expand-file-name "~/Dropbox/2TextBooks/.1-NegeleOrland-QuantumManyParticeSystems.pdf.bib")))
  (let* ((tmpfilepath (make-temp-file "alternative-bibtex-entry")))
    (find-file-other-frame existing-bibfile-path)
    ;; (if (not (member (get-buffer-window (get-buffer (file-name-nondirectory existing-bibfile-path)))
    ;;                  (window-list (selected-frame))))
    (progn
      (if (<= (length (window-list)) 1)
          (split-window-vertically)
          )
      (other-window 1)
      )
    ;;  )
    (if alternative-bibtex-entry-str
        (progn
          (find-file tmpfilepath)
          (insert alternative-bibtex-entry-str))
      (isbn-to-bibtex isbn tmpfilepath)
      (setq alternative-bibtex-entry-str (buffer-string))
      (if (and (/= 0 (length alternative-bibtex-entry-str))
               (= 0 (length
                     (with-current-buffer
                         (get-buffer (file-name-nondirectory existing-bibfile-path))
                       (buffer-string)))))
          (progn
            (if (yes-or-no-p
                 (concat existing-bibfile-path "'s shown buffer is empty. Fill it with the standard suggestion?"))
                (progn                  ;
                  (switch-to-buffer-other-window
                   (get-buffer (file-name-nondirectory existing-bibfile-path))) ;; or (other-window -1)
                  (insert alternative-bibtex-entry-str)
                  ;; (other-window -1)
                  )
              )
              ;; check if page-offset field is already included
            )
        )
      )
    )
  )

(defun bibtex-get-field-from-entry-under-cursor (field)
  (with-current-buffer "bibliography.bib"
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (field-value (org-ref-reftex-get-bib-field field entry)))
        field-value
        )
      )
    )
  )

(defun ff (arg)
  "Prompt user to enter a string, with input history support."
  (interactive
   (list
    (read-number "pdf page offset number: ")))
  (message "number is %s." (number-to-string arg))
  arg)


(defun diagnose-bib-entry-file-page-offset (&optional bibtexkey page)
  (interactive)
  "run in the context of a bib-buffer
   TODO check if there's a file-page-offset field in the bib entry you're hovering over.
   If not, or if it doesn't contain a value, ask to diagonose the pdf manually
   by opening it and asking for the value.
   There's probably also a way to do it automatically and semi-reliably... "
  ;; (unless bibfile-path (setq bibfile-path (expand-file-name "~/Dropbox/2TextBooks/.1-NegeleOrland-QuantumManyParticeSystems.pdf.bib")))
  ;; (unless pdf-path (setq pdf-path (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  (let* ((key (bibtex-get-field-from-entry-under-cursor "=key="))
         (file-page-offset-str (bibtex-get-field-from-entry-under-cursor "file-page-offset"))
         ;; returns "" if file-page-offset field is not there or has no value
         (file-page-offset (if (string= "" file-page-offset-str)
                               nil
                             (string-to-number file-page-offset-str)))
         )
    (if file-page-offset
        (progn
          (if (yes-or-no-p
               (concat "file-page-offset is set to " file-page-offset-str))
              (save-excursion
                (progn
                  ;; open up the pdf in a new frame on page 1 (first pdf page)
                  (let* ((filepath (bibtex-get-field-from-entry-under-cursor "filepath"))
                         red-string
                         number
                         (old-buffer (current-buffer)))
                    (if filepath
                        (progn
                          (open-pdf-document-new-frame filepath file-page-offset)
                          (setq number (call-interactively 'ff))
                          (delete-frame (selected-frame))
                          (switch-to-buffer-other-frame old-buffer)
                          (bibtex-set-field "file-page-offset" (number-to-string number))
                          )
                      (message "no filepath declared for " key)
                      )
                    )
                  )
                  )
                )
              )
      ;; else, ask to open the pdf to see
      (if (yes-or-no-p
           (concat "There's no file-page-offset field in " key
                   ". How 'bout opening up the PDF to manually find the offset?"))
          (save-excursion
            (progn
              ;; open up the pdf in a new frame on page 1 (first pdf page)
              (let* ((filepath (bibtex-get-field-from-entry-under-cursor "filepath"))
                     red-string
                     (old-buffer (current-buffer)))
                (if filepath
                    (progn
                      (open-pdf-document-new-frame filepath 1)
                      (setq number (call-interactively 'ff))
                      (delete-frame (selected-frame))
                      (switch-to-buffer-other-frame old-buffer)
                      (bibtex-set-field "file-page-offset" (number-to-string number))
                      )
                  (message "no filepath declared for " key)
                  )
                )
              )
              )
            )
      )
    )
  )

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


(global-set-key (kbd "C-, m") 'make-bibtex-file-for-pdf)
(global-set-key (kbd "C-, i") 'make-invisible)
(global-set-key (kbd "C-, o") 'search-nearest-link-and-open)
(global-set-key (kbd "C-, p") 'helm-browse-pdf-buffers)
(global-set-key (kbd "C-, i") 'make-invisible)
(global-set-key (kbd "C-, v") 'make-visible)
(global-set-key (kbd "C-2") 'helm-mini)  ;; select buffers with C-Space, delete selection with M-S-d


(defun bibtex-next-entry (&optional n)
  "Jump to the beginning of the next bibtex entry. N is a prefix
argument. If it is numeric, jump that many entries
forward. Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
                     (bibtex-beginning-of-entry)))
    (forward-char)
    (bibtex-next-entry))

  ;; search forward for an entry 
  (when 
      (re-search-forward bibtex-entry-head nil t (and (numberp n) n))
    ;; go to beginning of the entry
    (bibtex-beginning-of-entry)))


(defun bibtex-previous-entry (&optional n)
  "Jump to beginning of the previous bibtex entry. N is a prefix
argument. If it is numeric, jump that many entries back."
  (interactive "P")
  (bibtex-beginning-of-entry)
 (when 
     (re-search-backward bibtex-entry-head nil t (and (numberp n) n))
   (bibtex-beginning-of-entry)))

(defun jmax-bibtex-get-fields ()
  "Get a list of fields in a bibtex entry."
  (bibtex-beginning-of-entry)
  (remove "=type="
          (remove "=key="
                  (mapcar 'car (bibtex-parse-entry)))))

(defun jmax-bibtex-jump-to-field (field)
  "Jump to FIELD in the current bibtex entry"
  (interactive
   (list
    (ido-completing-read "Field: " (jmax-bibtex-get-fields))))
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (when
        ;; fields start with spaces, a field name, possibly more
        ;; spaces, then =
        (re-search-forward (format "^\\s-*%s\\s-*=" field) nil t))))
