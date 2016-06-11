; ------------- chapter 10 exercises
;
; 1. If x is a, y is b, and z is (c d), write backquoted expressions containing
; only variables that yield each of the following:
;
;  (a) ((C D) A Z)
;  (b) (X B C D)
;  (c) ((C D A) Z)
;
(let ((x 'a) (y 'b) (z (list 'c 'd)))
  (defun ch10-ex1-1 ()
    `(,z ,x z))
  (defun ch10-ex1-2 ()
    `(x ,y ,@z))
  (defun ch10-ex1-3 ()
    `((,@z ,x) z)))

; 2. Define if in terms of cond.

(defmacro ch-10-if (cnd body1 body2)
  `(cond (,cnd ,body1)
         (t ,body2)))

; 3. Define a macro that takes a number n followed by one or more expressions,
; and returns the value of the nth expression:
;
; > (let ((n 2))
;     (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
; 3

; 4. Define ntimes (page 167) to expand into a (local) recursive function
; instead of a do.

; 5. Define a macro n-of that takes a number n and an expression, and returns
; a list of n successive values returned by the expression:
;
; > (let ((i 0) (n 4))
;     (n-of n (incf i)))
; (1 2 3 4)

; 6. Define a macro that takes a list of variables and a body of code, and
; ensures that the variables revert to their original values after the body of
; code is evaluated.

; 7. What's wrong with the following definition of push?
;
; (defmacro push (obj lst)
;   `(setf ,lst (cons ,obj ,lst)))
;
; Give an example of a call where it would not do the same thing as the real
; push.

; 8. Define a macro that doubles its argument:
;
; > (let ((x 1))
;     (double x)
;     x)
; 2
