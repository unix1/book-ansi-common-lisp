; ------------- chapter 2 exercises

; 2.
; Give 3 distinct cons expressions that return (a b c)
; (cons 'a (cons 'b (cons 'c nil)))
; (cons 'a '(b c))
; (cons 'a (list 'b 'c))

; 3.
; Using car and cdr, define a function to return the 4th member of a list
(defun list-4th (lst)
  (car (cdr (cdr (cdr lst)))))

; get n-th member of the list
(defun list-nth (lst n)
  (list-nth-w lst n 0))

(defun list-nth-w (lst n current)
  (if (eql n current)
    (car lst)
    (list-nth-w (cdr lst) n (+ current 1))))

; 4.
; define a function that takes two arguments and returns the greater of the two
(defun get-greater (a b)
  (if (> a b) a b))

; 5.
; what do these functions do?
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))
; returns whether a given list contains a nil

(defun mystery (x y)
  (if (null y)
    nil
    (if (eql (car y) x)
      0
      (let ((z (mystery x (cdr y))))
        (and z (+ z 1))))))
; returns index of first occurrence of a value in a list

; 6.
; what could occur in place of x in each of the following exchanges?

; (a) > (car (x (cdr '(a (b c) d))))
;     B
; funcion that returns a first element of a list - i.e. car

; (b) > (x 13 (/ 1 0))
;     13
; or - so that 13 evaluates to true and 2nd argument, division by 0, is not evaluated

; (c) > (x #'list 1 nil)
;     (1)
; apply - it applies the (list 1), the last nil/empty list argument is
; disregarded, even though apply requires the last argument be a list

; 7.
; Using only operators introduced in this chapter, define a function that takes
; a list as an argument and returns true if one of its elements is a list.

(defun list-is-multi-dim (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
          t
          (list-is-multi-dim (cdr lst)))))

; 8.
; Give iterative and recursive definitions of a function that
; (a) takes a positive integer and prints that many dots.
; (b) takes a list and returns the number of times the symbol a occurs in it.

; (a) recursive
(defun print-dots-r1 (n)
  (if (eql n 0)
    'done
    (progn (print ".")
           (print-dots-r (- n 1)))))

; (a) recursive with/TCO
(defun print-dots-r2 (n)
  (print (get-dots "." n "")))

(defun get-dots (chr n acc)
  (if (eql n 0)
    acc
    (get-dots chr (- n 1) (concatenate 'string acc chr))))

; (a) iterative
(defun print-dots-i1 (n)
  (do ((i 0 (+ i 1)))
      ((> i n) 'done)
    (print ".")))

; (b) recursive w/TCO
(defun list-occurs-r1 (lst item)
  (list-occurs-r1-get lst item 0))

(defun list-occurs-r1-get(lst item idx)
  (if (null lst)
    idx
    (let ((idx-new (if (eql item (car lst)) (+ idx 1) idx)))
      (list-occurs-r1-get (cdr lst) item idx-new))))

; (b) iterative
(defun list-occurs-i1 (lst item)
  (let ((times 0))
    (dolist (obj lst)
      (if (eql item obj)
        (setf times (+ times 1))))
    times))

; 9. A friend is trying to write a function that returns the sum of all the
; non-nil elements in a list. He has written two versions of this function, and
; neither of them work. Explain what's wrong with each, and give a correct
; version:

; (a) (defun summit (lst)
;       (remove nil lst)
;       (apply #'+ lst))
;
; remove returns the result, doesn't update in place
(defun summit1(lst)
  (let ((lst2 (remove nil lst)))
    (apply #'+ lst2)))

; (b) (defun summit (lst)
;       (let ((x (car lst)))
;         (if (null x)
;             (summit (cdr lst))
;             (+ x (summit (cdr lst))))))
;
; doesn't check when list ends
(defun summit2(lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit2 (cdr lst))
            (+ x (summit2 (cdr lst)))))))
