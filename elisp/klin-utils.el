
(defun klin-try-to-fill-all-pdf-stuff-in-1-isbn ()
  "procedure to fill out a bibtex's entries in terms of
   pdf data"
  (interactive)

  ;; re-parse the bib buffer
  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
  (bibtex-parse-keys)

  ;; if it's empty, try to find the pdf from the file name first,
  ;; in order to get to the isbn, which enables you to get the
  ;; title, author, year, publisher data from the internet
  (let* (pdf-filepath)
    (if (eq (length (bibtex-global-key-alist)) 0)
        (progn
          (setq pdf-filepath
                (klin-get-pdf-filepath-for-bibtex-entry))
          )
      )
    ;; now that we have the file path, open the pdf to find the isbn
    (message "We'll open up the pdf now. You could try to find the isbn.")
    (sleep-for 2)
    (find-file-other-frame pdf-filepath)
    )
  )

(defun klin-try-to-fill-all-pdf-stuff-in-2-pdfstuff ()
  "assuming you have now a boilerplate entry, e.g. from
   (isbn-to-bibtex), you can copy it to your main file;
   then, this function calls functions to set the pdf
   related fields of your bibtex entry"
  (interactive)
  (let* ((filepath-field-str
         (bibtex-get-field-from-entry-under-cursor
          "filepath" (current-buffer)))
         (file-page-offset-field-str
          (bibtex-get-field-from-entry-under-cursor
           "file-page-offset" (current-buffer)))
         )
    (unless (or (not (string= "" filepath-field-str)) (not filepath-field-str))
      (fix-filepath-field))
    (unless (or (not (string= "" file-page-offset-field-str))
                (not file-page-offset-field-str))
      (fix-file-page-offset))
    )
  )

(defun klin-get-pdf-filepath-for-bibtex-entry (&optional key)
  "runs inside a .bib buffer
   try out a few methods to get to the pdf"
  (interactive)

  ;; re-parse the bib buffer
  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
  (bibtex-parse-keys)

  (setq pdf-filepath
        (helm-read-file-name
         (concat "what is the corresponding pdf?"
                 (progn
                   (if (or (not (string= "" key)) (not key))
                       "(key: not set)"
                     (concat "(key: " key " )")
                     ))
                 ": ")
         :initial-input
         (let* ((filepath-field
                 (bibtex-get-field-from-entry-under-cursor
                  "filepath" (current-buffer)))
                (filename-guess-from-bibfile-name
                  (klin-bibtex-filename-to-pdf-filename (buffer-name)))
                (standard-folder-path
                 (expand-file-name "~/Dropbox/2TextBooks/"))
                (filepath-guess-from-bibfile-name-standard-folder
                 (concat standard-folder-path
                         filename-guess-from-bibfile-name))
                )

           (if (and (not (string= filepath-field ""))
                    (file-exists-p (expand-file-name filepath-field)))
               (expand-file-name filepath-field)
             (if (file-exists-p
                  filepath-guess-from-bibfile-name-standard-folder)
                 filepath-guess-from-bibfile-name-standard-folder
               (if (file-exists-p standard-folder-path)
                   standard-folder-path
                 ""
                 )
               )
             )
           )
         )
        )
  )

(defun klin-bibtex-filename-to-pdf-filename (bibtex-filename)
  (interactive)
  (let* ( ;; (filename ".myname.pdf.bib") ;; test (checked using re-builder)
         (filename bibtex-filename)
         )
    (string-match "^\.\\(+?.*\\)\.bib$" filename)
    (match-string 1 filename))
  )

(defun klin-pdf-filename-to-bibtex-filename (pdf-filename)
  (interactive)
  (concat "." pdf-filename ".bib")
  )

(defun klin-pdf-filepath-to-bibtex-filepath (pdf-filepath)
  (interactive)
  (let* ((pdf-filename (file-name-nondirectory pdf-filepath))
         (bibtex-filename (klin-pdf-filename-to-bibtex-filename pdf-filename))
         (bibtex-filepath (concat (file-name-directory pdf-filepath) bibtex-filename))
         )
    bibtex-filepath
    )
  )

(defun klin-ask-pdf-offset-number (arg)
  "Prompt user to enter a string, with input history support."
  (interactive
   (list
    (read-number "pdf page offset number: ")))
  (message "number is %s." (number-to-string arg))
  arg)

(defun get-all-pdf-buffers ()
  (interactive)
  (setq pdfbuffers (make-list 0 0))
  (setq i 0)
  (while (< i (length (buffer-list)))
    (setq bufname (buffer-name (nth i (buffer-list))))
    (if (string-match-p ".pdf$" bufname)
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

(defun open-pdf-document-new-frame (&optional filepath page)
  "open a pdf file in a new frame"
  (unless page (setq page 1))
  (unless filepath (setq filepath (expand-file-name "~/Dropbox/2TextBooks/1-NegeleOrland-QuantumManyParticeSystems.pdf")))
  (progn
    (find-file-other-frame filepath)
    (pdf-view-goto-page page)))

(defun kill-frame-and-buffers-within ()
  (interactive)
  ;; get the buffers within the currently selected frame
  (let* ((buffers-within-frame (cl-delete-duplicates (mapcar #'window-buffer (window-list))))
         (frame (selected-frame))
         (remaining (delq nil (mapcar (lambda (buf)
                                        (let ((this-window-buffer (get-buffer-window buf)))
                                          (if (not (kill-buffer buf))
                                              buf
                                            (delete-window this-window-buffer)
                                            nil)))
                                        buffers-within-frame)))
         (intersection (cl-intersection buffers-within-frame remaining))
         (killed-buffers (cl-set-exclusive-or buffers-within-frame remaining))
         )
    (if (= (length remaining) 0)
      (delete-frame)  ;; defaults to selected frame
      )
    )
  )
