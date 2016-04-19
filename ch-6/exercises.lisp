; ------------- chapter 6 exercises
;
; 1. Defina a version of tokens (page 67) that takes :test and :start arguments
; defaulting to #'constituent and 0 respectively.
(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str
                               :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str :test test :start p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))

; 2. Define a version of bin-search (page 60) that takes :key, :test, :start
; and :end argumentswith the usual meanings and defaults.

; 3. Define a function that takes any number of arguments and returns the
; number of arguments passed to it.
(defun num-args (&rest args)
  (length args))
