
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
