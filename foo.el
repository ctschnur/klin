(defun wh/help-hello-world ()
  (interactive)
  (with-help-window (help-buffer)
    (princ "foo_bar is a function.\n\nIt does stuff.")
    (shrink-window 1)))

(defun button-pressed (button)
  (message (format "Button pressed!")))

(define-button-type 'custom-button
  'action 'button-pressed
  'follow-link t
  'help-echo "Click Button"
  'help-args "test")

(make-button 1 10 :type 'custom-button)
