(defun ask-number (prompt)
  ;(format t prompt)
  (let ((val (prompt-read prompt)))
    (if (numberp val)
      val
      (ask-number "try again"))))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read))
