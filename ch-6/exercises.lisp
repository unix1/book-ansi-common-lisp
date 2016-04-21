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
; and :end arguments with the usual meanings and defaults.
(defun bin-search (obj vec &key (key #'identity) (test #'eql) (start 0) (end nil))
  (let ((len (if (null end)
                 (- (length vec) 1)
                 end)))
    (and (not (zerop len))
         (finder obj vec key test start len))))

(defun finder (obj vec key test start end)
  (let ((range (- end start)))
    (if (zerop range)
        (if (funcall test obj (funcall key (aref vec start)))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (funcall key (aref vec mid))))
            (if (< obj obj2)
                (finder obj vec key test start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec key test (+ mid 1) end)
                    obj)))))))

; 3. Define a function that takes any number of arguments and returns the
; number of arguments passed to it.
(defun num-args (&rest args)
  (length args))

; 4. Modify most (page 105) to return, as two values, the two highest-scoring
; elements of a list.

; 5. Define remove-if (no keywords) in terms of filter (page 105)

; 6. Define a function that takes one argument, a number, and returns the
; greatest argument passed to it so far.
(defparameter *greatest-arg* nil)
(defun greatest-arg (n)
  (let ((result (if (null *greatest-arg*)
                  n
                  (max *greatest-arg* n))))
    (setf *greatest-arg* result)))

; 7. Define a function that takes one argument, a number, and returns true if
; it is greater than the argument that was passed to the function the last time
; it was called. The function should return nil the first time it is called.
(defparameter *greater-than-last-arg* nil)
(defun greater-than-last-arg? (n)
  (declare (special *greater-than-last-arg*))
  (let ((result (and (not (null *greater-than-last-arg*))
                  (> n *greater-than-last-arg*))))
    (setf *greater-than-last-arg* n)
    result))

; 8. Supposed expensive is a function of one argument, an integer between 0 and
; 100 inclusive, that returns the result of a time-consuming computation.
; Define a function frugal that returns the same answer, but only calls
; expensive when given an argument it has not seen before.

; 9. Define a function like apply, but where any number printed out before it
; returns will be printed, by default, in octal (base 8).
