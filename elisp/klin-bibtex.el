
;; for consecutive processing of individual bibtex files assoc. to single pdfs
(setq bib-processing-bibfiles-frames nil)
(setq bib-processing-currently-processing nil)

(defun isbn-to-bibtex-ottobib-insert (isbn)
  "called within a .bib file, inserts at point"
  (interactive "sISBN: ")
  ;; (unless isbn (setq isbn "0195117972"))
  (let* ((tmpfilename (make-temp-file "ottobib")
          ;; "/home/chris/Dropbox/stuff/1Book/elisp/bar"
          )
         (bibtex-text
          (let* (beginning
                 text-inbetween
                 ending)
            (url-copy-file (concat "https://www.ottobib.com/isbn/ " isbn "/bibtex") tmpfilename t)
            (with-temp-buffer
              (insert-file-contents tmpfilename)
              (goto-char (point-min))
              (setq beginning (re-search-forward "textarea.+?>"))
              (setq ending (progn (re-search-forward "<\/textarea>")
                                  (re-search-backward "<\/text")))
              (setq text-inbetween (buffer-substring-no-properties beginning ending))
              )
          )
           )
         )
    ;; (with-current-buffer "bar"
    ;;   (erase-buffer)
    ;;   (save-buffer))
         (insert bibtex-text)
    )
  )

(defun bibtex-get-field-from-entry-under-cursor (field bibfile-buffer)
  (with-current-buffer bibfile-buffer
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (field-value (org-ref-reftex-get-bib-field field entry)))
        field-value
        )
      )
    )
  )

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
      )
    )
  )


(defun open-bibtex-document-on-page (bibtexkey page)
  (let* ((file-page-offset (string-to-number (klin-bibtex-get-field "file-page-offset" bibtexkey)))
         (filepath (klin-bibtex-get-field "filepath" bibtexkey))
         (page (- (+ page file-page-offset) 1)))
    (open-pdf-document-new-frame filepath page)
    )
  )

(defun make-bibtex-file-for-pdf (&optional pdfpath isbn doi)
  (interactive)
  ;; (unless pdfpath (setq pdfpath (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  ;; (unless isbn (setq isbn "0-7382-0052-2"))
  (let* ((basedir (file-name-directory pdfpath))
         (filename (file-name-nondirectory pdfpath))
         (bibtexfilename (concat "." filename ".bib")) ;; "hidden" file
         (bibfilepath (concat basedir bibtexfilename))
         )

    ;; check if pdf actually exists
    (unless (file-exists-p pdfpath)
      (unless (yes-or-no-p "pdf file doesn't exist, continue anyway?")
        (error "pdf file doesn't exist, chose to quit")
        ))

    ;; this appends to a file (and creates one if there is none)
    (unless bibfilepath (setq bibfilepath (expand-file-name "~/Dropbox/stuff/1Book/testfile.txt")))
    (with-temp-buffer (write-region "" nil bibfilepath 'append))
    (side-by-side-bibtex-edit bibfilepath nil)
    )
  )

(defun on-bibtex-processing-frame-close-hf (&optional frame)
  "If the current frame of the bibtex batch processing is closed, you move on
   to the next frame in the list. FRAME is the frame that is about to be deleted"
  (interactive)
  (unless frame (setq frame (selected-frame)))
  (message (concat "calling on-bibtex-processing-frame-close-hf on frame "
           (prin1-to-string frame)))
  (let* (;; (fitting-tuple (car (rassoc frame bib-processing-bibfiles-frames)))
         (fitting-tuple
          (car (delq nil (mapcar (lambda (cur-tuple)
                                   (if (eq (nth 1 cur-tuple)
                                           frame)
                                       cur-tuple
                                     nil
                                     )
                                   )
                            bib-processing-bibfiles-frames
         )))))
         ;; if that frame is inside the bib-processing-bibfiles-frames,
         ;; delete the tuple from the list
    (if fitting-tuple
        (let* ((intended-bibfile-path (car fitting-tuple)))
          ;; close the intended-bibfile-buffer
          (let ((intended-bibfile-buffer (get-buffer (file-name-nondirectory intended-bibfile-path))))
            (if intended-bibfile-buffer
                (kill-buffer intended-bibfile-buffer)
              )
            )
          ;; if the intended-bibfile is empty now (maybe after saving), delete it
          (if (string= "" (with-temp-buffer
                            (insert-file-contents intended-bibfile-path) (buffer-string)))
              (delete-file intended-bibfile-path)
            )
          )
      )
    ;; remove the currently processed tuple from the list
    (setq bib-processing-bibfiles-frames
          (delq fitting-tuple bib-processing-bibfiles-frames))
    ;; open up a new frame with side-by-side-bibtex-edit,
    ;; processing the next (car) tuple

    (if (> (length bib-processing-bibfiles-frames) 0)
        (let* ((next-tuple (car bib-processing-bibfiles-frames))
               (bibfile-path (car next-tuple))
               )
          ;; open up the next one
          (side-by-side-bibtex-edit bibfile-path)
          )
      ;; if bib-processing-bibfile-frames is empty,
      ;; set the state variable to not processing any more
      (setq bib-processing-currently-processing nil)
      )
    )
    ;; then, let it proceed to delete the frame
  )

;; (defun after-bibtex-processing-close-hf (frame)
;;   "runs after the frame has been closed, launching a new frame;
;;    if the new frame runs an interactive function,
;;    this could be one way to completely get rid of the old frame
;;    before the interaction starts"
;;   (interactive)
;; 
;;   (unless frame (setq frame (selected-frame)))
;;   (message (concat "calling after-bibtex-processing-frame-close-hf on frame "
;;                    (prin1-to-string frame)))
;; 
;;   (if (> (length bib-processing-bibfiles-frames) 0)
;;       (let* ((next-tuple (car bib-processing-bibfiles-frames))
;;              (bibfile-path (car next-tuple)))
;;         ;; open up the next one
;;         (side-by-side-bibtex-edit bibfile-path)
;;         )
;;     ;; else, set it to not processing any more
;;     (setq bib-processing-currently-processing nil)
;;     )
;;   ;; do that only for the relevant frames,
;;   ;; so remove this same function right away from the hook
;;   (delete 'after-bibtex-processing-frame-close-hf 'delete-frame-functions)
;;   )


(defun side-by-side-bibtex-edit (&optional intended-bibfile-path
                                           alternative-bibtex-entry-str)
  "bib file already exists somewhere, don't overwrite it, complete it with another
   alternative bibtex entry delivered to the function.
   removes the existing-bibfile if it remains empty
   "
  (interactive)
  ;; (unless intended-bibfile-path (setq intended-bibfile-path (expand-file-name "~/Dropbox/2TextBooks/.1-NegeleOrland-QuantumManyParticeSystems.pdf.bib")))

  ;; open the existing bibfile
  (let* ((tmpfilepath (make-temp-file "alternative-bibtex-entry")))
    (find-file-other-frame intended-bibfile-path)

    ;; ;; split it in two to get side-by-side view with another buffer
    ;; (progn
    ;;   (if (<= (length (window-list)) 1)
    ;;       (split-window-vertically))
    ;;   (other-window 1))

    (if alternative-bibtex-entry-str
        ;; if some text was given, open a temp file and insert it
        (progn
          (find-file tmpfilepath)
          (insert alternative-bibtex-entry-str)
          )
      ;; else: propose the opportunity to manually run isbn-to-bibtex
      ;; to get a suggestion
      (message "you can run e.g. ask-for-isbn-suggest-and-insert to get a suggestion")
      ;; you could run e.g. ask-for-isbn-suggestion-and-insert ()
      )

    ;; for consecutive processing of individual bibtex files assoc. to single pdfs:
    ;; change the frame value assoc to the current intended-bibfile-path
    (let* ((replace-with-tuple
            `(,intended-bibfile-path ,(selected-frame)))
           (mylist bib-processing-bibfiles-frames))
      (let* ((i 0))
        (while (< i (length mylist))
          (if (string= intended-bibfile-path (car (nth i mylist)))
              (progn
                (setf (nth i mylist) replace-with-tuple)
                (setq bib-processing-currently-processing t)
              )
            )
          (setq i (+ i 1))
          )
        )
      )

    ;; add a function to delete-frame-functions that will handle the transition:
    ;; when kill-frame-and-buffers-within is called at some point, that will
    ;; kill the previous frame and open up the next one in the list
    (add-to-list 'delete-frame-functions 'on-bibtex-processing-frame-close-hf)
    )
  )

(defun ask-for-isbn-suggestion-and-insert ()
  "an automated way of filling an empty bib file with an isbn
   suggestion, if that bibfile is the only window of it's frame"
  (interactive)
  (let* (;;(tmpfilepath (make-temp-file "isbn-suggestion"))
         (isbn (read-string "ISBN? (return to continue):"))
         (before-isbncmd-buf (current-buffer))
         after-isbncmd-buf
         ;;(tmpfile-window (split-window-below))
         )

    ;; goes off to the internet
    ;; if sth is delivered back, it creates a new buffer,
    ;; else, it just keeps the current buffer, but in split view
    (isbn-to-bibtex isbn (buffer-file-name (current-buffer)))

    ;; so now, if the buffer has changed, it presumably has gotten sth. back
    ;; if the buffer has changed, check if it actually contains anything
    ;; if not, you just close that window
    ;; (setq after-isbncmd-buf (current-buffer))
    ;; (if (not (eq after-isbncmd-buf before-isbncmd-buf))
    ;;     (if (string= "" (with-current-buffer after-isbncmd-buf (buffer-string)))
    ;;         (progn
    ;;           ;; delete that window and that buffer
    ;;           (delete-window (get-buffer-window after-isbncmd-buf))
    ;;           (kill-buffer after-isbncmd-buf)
    ;;           )
    ;;       ;; else, nothing happens
    ;;       )
      ;; else, delete that window and that buffer
      ;; (delete-window (get-buffer-window after-isbncmd-buf))
      ;; don't kill the buffer because presumably you want to continue editing it
      ;; (kill-buffer after-isbncmd-buf)
      )

    ;; if an actual suggestion could be found by isbn, and the intended buffer is empty,
    ;; ask to insert it there
    ;; (let* ((after-isbncmd-suggested-entry-str
    ;;         (with-current-buffer after-isbncmd-buf (buffer-string))))
    ;;     (if (and (/= 0 (length after-isbncmd-suggested-entry-str))
    ;;              (= 0 (length
    ;;                    (with-current-buffer
    ;;                        (get-buffer (file-name-nondirectory intended-bibfile-path))
    ;;                      (buffer-string)))))
    ;;         (progn
    ;;           (if (yes-or-no-p
    ;;                (concat intended-bibfile-path
    ;;                        "'s shown buffer is empty. Fill it with the standard suggestion?"))
    ;;               (progn
    ;;                 (with-current-buffer before-isbncmd-buf
    ;;                   (insert after-isbncmd-suggested-entry-str)
    ;;                   )
    ;;                 )
    ;;             )
    ;;           )
    ;;       )
    ;;     )
    ;; )
  )

;; (defun diagnose-bib-entry-file-page-offset (&optional bibtexkey page)
;;   "run in the context of a bib-buffer"
;;   (interactive)
;;   (let* ((bibfile-buffer (current-buffer))
;;          (key (bibtex-get-field-from-entry-under-cursor "=key=" bibfile-buffer))
;;          (file-page-offset-str
;;           (bibtex-get-field-from-entry-under-cursor
;;            "file-page-offset" bibfile-buffer))
;;          ;; returns "" if file-page-offset field is not there or has no value
;;          (file-page-offset (if (string= "" file-page-offset-str)
;;                                nil
;;                              (string-to-number file-page-offset-str)))
;;          )
;;     (if file-page-offset
;;       (if (yes-or-no-p
;;            (concat "There's no file-page-offset field in " key
;;                    ". How 'bout opening up the PDF to manually find the offset?"))
;;           (save-excursion
;;             (progn
;;               ;; open up the pdf in a new frame on page 1 (first pdf page)
;;               (let* ((filepath (bibtex-get-field-from-entry-under-cursor
;;                                 "filepath" bibfile-buffer))
;;                      red-string)
;;                 (if (not (string= "" filepath))
;;                     (fix-file-page-offset-field key)
;;                   (fix-filepath-field key)
;;                     )
;;                   )
;;                 )
;;               )
;;               )
;;             )
;;       )
;;     )
;; )

(defun fix-file-page-offset (&optional key)
  "called in a .bib buffer"
  (interactive)
  (let* ((bib-buffer (current-buffer))
         (bib-window (selected-window))
         pdf-buffer
         pdf-frame
         pdf-filepath
         offset-number)
    (unless key (setq key (bibtex-get-field-from-entry-under-cursor
                           "=key=" bib-buffer)))
    ;; open up a pre-populated helm menu for pdf-filepath
    ;; in order to open up the pdf for checking

    (setq pdf-filepath
          (helm-read-file-name
           (concat "pdf filepath for " key ": ")
           :initial-input
           (let* ((filepath-field
                   (bibtex-get-field-from-entry-under-cursor
                    "filepath" bib-buffer))
                  (standard-folder-path
                   (expand-file-name "~/Dropbox/2TextBooks/")))
             (if (file-exists-p (expand-file-name filepath-field))
                 (expand-file-name filepath-field)
               (if (file-exists-p standard-folder-path)
                   standard-folder-path
                 ""
                 )
               )
             )
           )
          )
    
    (open-pdf-document-new-frame (expand-file-name pdf-filepath) 1)
    (setq pdf-buffer (current-buffer))
    (setq pdf-frame (selected-frame))

    (setq offset-number (call-interactively 'klin-ask-pdf-offset-number))
    (delete-frame pdf-frame)
    (pop-to-buffer bib-buffer)
    (bibtex-set-field "file-page-offset" (number-to-string offset-number))
    )
  (bibtex-clean-entry)
  (bibtex-fill-entry)
  )

(defun fix-filepath-field (&optional key)
  "called in a .bib buffer"
  (interactive)
  (let* (filepath)
    (unless key (setq key (bibtex-get-field-from-entry-under-cursor
                           "=key=" (current-buffer))))
      (setq filepath (klin-get-pdf-filepath-for-bibtex-entry))
      (bibtex-set-field "filepath"
                        (s-replace
                         (substitute-in-file-name "$HOME") ;; expl. home dir, to replace
                         "~" ;; what to replace it with
                         (substitute-in-file-name
                          (expand-file-name filepath)) ;; total string
                         )
                        )
    )
  (bibtex-clean-entry)
  (bibtex-fill-entry)
  )


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

(defun check-pdfs-for-bib-file (&optional filepaths check-all)
  "mark some pdfs in dired, then"
  (interactive)
  (unless filepaths
    (setq filepaths (helm-read-file-name
                     "mark some pdfs: "
                     :initial-input "~/Dropbox/"
                     :marked-candidates t)))

  (unless check-all (setq check-all nil))

  (when (> (length bib-processing-bibfiles-frames) 0)
    ;; (when (yes-or-no-p "some bib files are still waiting to be processed. Skip them?")
    (message "some bib files were still in the queue, waiting to be processed. We skip them, reinitializing the processing queue.")
    (setq bib-processing-bibfiles-frames nil))

  ;; assign the list of bibfiles to process to bib-processing-bibfiles-frames
  ;; fill frames with nil for the beginning
  (let* ((as (mapcar (lambda (arg) (klin-pdf-filepath-to-bibtex-filepath arg)) filepaths))
         (prepared-bibfiles-frames-list
          (mapcar (lambda (ai)
                    (let ((temp-as-bs nil))
                      (setq temp-as-bs (append temp-as-bs `(,ai ,nil)))))
                  as)))
    (setq bib-processing-bibfiles-frames
          (append bib-processing-bibfiles-frames prepared-bibfiles-frames-list))
    )

  ;; (when (> (length bib-processing-bibfiles-frames) 0)
  ;;     (when (yes-or-no-p "pdf file doesn't exist, continue anyway?")
  ;;         (message "hi")))

  ;; setup the global bib-processing-bibfiles-frames list with the filepaths

  ;; start with the first one, that initiates it; after closing that one,
  ;; there is a hook to open the 2nd one and so on.
  (let ((pdf-filepath (nth 0 filepaths)))
    (make-bibtex-file-for-pdf pdf-filepath)
    )
  )
