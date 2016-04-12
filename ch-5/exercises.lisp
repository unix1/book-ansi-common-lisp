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

; 5. Define iterative and recursive versions of a function that takes an object
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

(defun precedes-i (x v)
  (let ((acc nil))
    (do ((i 1 (+ i 1)))
        ((>= i (array-dimension v 0)) 'done)
      (if (eql (aref v i) x)
          (push (aref v (- i 1)) acc)))
    (remove-duplicates acc)))

; 6. Define iterative and recursive versions of a function that takes an object
; and a list, and returns a new list in which the object appears between each
; pair of elements in the original list:
;
; > (intersperse '- '(a b c d))
; (A - B - C - D)
(defun intersperse-r (obj lst)
  (intersperse-r-raw obj lst nil))

(defun intersperse-r-raw (obj lst acc)
  (if (null (cdr lst))
      (reverse (cons (car lst) acc))
      (intersperse-r-raw obj (cdr lst) (cons obj (cons (car lst) acc)))))

(defun intersperse-i (obj lst)
  (let ((acc nil))
    (dolist (item lst)
      (if (null (cdr lst))
          (setf acc (cons item acc))
          (setf acc (cons obj (cons item acc)))))
    (reverse (cdr acc))))

; 7. Define a function that takes a list of numbers and returns true iff the
; difference between each successive pair of them is 1, using:
;
; (a) recursion
; (b) do
; (c) mapc and return

; 8. Define a single recursive function that returns, as two values, the
; maximum and minimum elements of a vector.
(defun min-max (v)
  (min-max-raw v 1 (list (aref v 0) (aref v 0))))

(defun min-max-raw (v cur acc)
  (if (eql cur (array-dimension v 0))
      (values (car acc) (car (cdr acc)))
      (cond ((< (aref v cur) (car acc))
                (min-max-raw v (+ cur 1) (cons (aref v cur) (cdr acc))))
            ((> (aref v cur) (car (cdr acc)))
                (min-max-raw v (+ cur 1) (cons (car acc) (cons (aref v cur) nil))))
            (t (min-max-raw v (+ cur 1) acc)))))

; 9. The program in Figure 3.12 continues to search as the first complete
; path works its way through the queue. In broad searches this would be a
; problem.
;
; (a) Using catch and throw, modify the program to return the first complete
; path as soon as it is discovered.
;
; (b) Rewrite the program to do the same thing without using catch and throw.
