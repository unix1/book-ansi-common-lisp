; ------------- chapter 5 exercises
;
; 1. Translate following expressions into equivalent expressions that don't use
; let or let*, and don't cause the same expression to be evaluated twice.
;
; (a) (let ((x (car y)))
;        (cons x x))
;((lambda (x) (cons x x))
; (car y))

; (b) (let* ((w (car x))
;            (y (+ w z)))
;       (cons w y)
;((lambda (w z) (cons w (+ w z)))
; (car x)
; z)

; 2. Rewrite mystery (page 29) to use cond
(defun mystery (x y)
  (cond ((null y) nil)
        ((eql (car y) x) 0)
        (t (let ((z (mystery x (cdr y))))
             (and z (+ z 1))))))

; 3. Define a function that returns a square of its argument, and which does
; not compute the square if the argument is a positive integer less than or
; equal to 5.

(defun weird-square (x)
  (if (and (> x 0) (< x 6) (eql (mod x 1) 0))
      nil
      (* x x)))

; 4. Rewrite num-month (Figure 5.1) to use case instead of svref.

; 5. Define iterative and recursive version of a function that takes an object
; and vector v, and returns a list of all the objects that immediately precede
; x in v:
;
; > (precedes #\a "abracadabra")
; (#\c #\d #\r)

(defun precedes-r (x v)
  (precedes-r-raw x v 1 nil))

(defun precedes-r-raw (x v cur-pos acc)
  (let ((match-pos (position x v :start cur-pos)))
       (if (null match-pos)
           (remove-duplicates acc)
           (precedes-r-raw x v (+ match-pos 1) (cons (aref v (- match-pos 1)) acc)))))
