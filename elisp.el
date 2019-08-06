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


(defun org-ref-get-zotero-pdf-filename (key)
    "Return the pdf filename indicated by zotero file field.
Argument KEY is the bibtex key."
    (let* ((results (org-ref-get-bibtex-key-and-file key))
           (bibfile (cdr results))
           entry)
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key nil 0)
        (setq entry (bibtex-parse-entry))
        (let ((e (org-ref-reftex-get-bib-field "file" entry)))
          (if (> (length e) 4)
              (let ((clean-field (replace-regexp-in-string "/+" "/" e)))
                (let ((first-file (car (split-string clean-field ";" t))))
                  (concat org-ref-pdf-directory first-file)))
            (message "PDF filename not found."))))))

(defun org-ref-get-zotero-pdf-page-offset (key)
    "Return the pdf's page offset (from where arabic numbering starts) indicated by zotero file field.
Argument KEY is the bibtex key."
    (let* ((results (org-ref-get-bibtex-key-and-file key))
           (bibfile (cdr results))
           entry)
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key nil 0)
        (setq entry (bibtex-parse-entry))
        (let ((e (org-ref-reftex-get-bib-field "file-page-offset" entry)))
          (if t
              (let ((clean-field (replace-regexp-in-string "/+" "/" e)))
                (let ((first-file (car (split-string clean-field ";" t))))
                  (concat org-ref-pdf-directory first-file)))
            (message "PDF offset not found."))))))

;; (setq file-page-offset (org-ref-get-zotero-pdf-page-offset "elb05"))
;; (setq pdf-filepath (org-ref-get-zotero-pdf-filename "elb05"))
;; great, this works

(defun open-bibtex-pdf-at-point (cite-str)
  (setq strparts (split-string cite-str ":"))
  (setq key (nth 1 strparts))
  (setq page (string-to-number (nth 2 strparts)))
  (setq file-page-offset (string-to-number (org-ref-get-zotero-pdf-page-offset key)))
  (setq pdf-filepath (org-ref-get-zotero-pdf-filename key))
  (progn
    (find-file-other-frame pdf-filepath)
    (pdf-view-goto-page (- (+ page file-page-offset) 1))))

 ; (open-bibtex-pdf-at-point "cite:elb05:30")

(defun read-point-string-and-trigger-opening ()
  (setq cite-str (concat "cite:" (org-ref-get-bibtex-key-under-cursor)))
  (open-bibtex-pdf-at-point cite-str)

;; (define-key org-mode-map (kbd "C-c o") 'read-point-string-and-trigger-opening)

(defun get-description-at-point ()
  (interactive)
  (setq link (org-element-context))
  (setq buf-substr (buffer-substring-no-properties
                    (org-element-property :contents-begin link)
                    (org-element-property :contents-end link)))))

(defun get-link-text-from-under-cursor ()
  (setq re "\\[\\[\\(.*?\\):\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
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
       (setq nearestlink-string nextlink-match-string)))

   ;; apply the regex again to the nearest string and copy the data
   (string-match re nearestlink-string)
   (list (match-string 1 nearestlink-string)
         (match-string 2 nearestlink-string)
         (match-string 3 nearestlink-string)))

(defun open-bibtex-document-on-page (bibtexkey page)
  (setq file-page-offset (string-to-number (org-ref-get-zotero-pdf-page-offset bibtexkey)))
  (setq pdf-filepath (org-ref-get-zotero-pdf-filename bibtexkey))
  (progn
    (find-file-other-frame pdf-filepath)
    (pdf-view-goto-page (- (+ page file-page-offset) 1))))

(defun printlist (mylist)
  (setq hi (nth 0 mylist)))

(defun openlink (mylist)
 (setq linktyp (nth 0 mylist))
 (setq bibtexkey (nth 1 mylist))
 (setq description (nth 2 mylist))
 (string-match "p\\.\\s-*\\([0-9]*\\)" description)
 (setq page-str (match-string 1 description))
 (open-bibtex-document-on-page bibtexkey (string-to-number page-str)))

(defun search-next-link-and-open ()
  (setq link-text-under-cursor (get-link-text-from-under-cursor))
  (openlink link-text-under-cursor))

(progn
  (string-match "p\\.\\s-*\\([0-9]*\\)" description)
  (setq page-str (match-string 1 description))
  (open-bibtex-document-on-page bibtexkey (string-to-number page-str)))
