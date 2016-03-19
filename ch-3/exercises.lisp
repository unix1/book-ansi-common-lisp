; ------------- chapter 3 exercises
;
; 2. Write a version of union that preserves the order of elements in the
; original lists
; > (list-union-ordered '(a b c) '(b a d))
; (A B C D)

(defun list-union-ordered (lst1 lst2)
  (list-union-ordered-raw lst2 (reverse lst1)))

(defun list-union-ordered-raw (lst2 acc)
  (if (null lst2)
      (reverse acc)
      (list-union-ordered-raw (cdr lst2) (pushnew (car lst2) acc))))

; 3. Define a function that takes a list and returns a list indicating the
; number of times each (eql) element appears, sorted from most common to least
; common.
;
; > (occurrences '(a b a d a c d c a))
; ((A . 4) (C . 2) (D . 2) (B . 1))

(defun occurrences (lst)
  (sort (occurrences-lib lst nil) #'> :key #'cdr))

(defun occurrences-lib (lst acc)
  (if (null lst)
      acc
      (if (null (assoc (car lst) acc))
          (occurrences-lib (cdr lst) (cons (cons (car lst) 1) acc))
          (let ((cur (assoc (car lst) acc))
                (new (cons (car lst) (+ (cdr (assoc (car lst) acc)) 1))))
            (occurrences-lib (cdr lst) (substitute new cur acc :test #'equal))))))

; 4. Why does (member '(a) '((a) (b))) return nil?
; Because by default member compares objects using eql

; 5. Suppose a function pos+ takes a list and returns a list of each element
; plus its position:
;
; > (pos+ '(7 5 1 4))
; (7 6 3 7)
;
; Define this function using (a) recursion, (b) iteration, (c) using mapcar.

; (a) recursion
(defun pos+r (lst)
  (reverse (pos+raw lst 0 nil)))

(defun pos+r-raw (lst pos acc)
  (if (null lst)
      acc
      (pos+r-raw (cdr lst) (+ pos 1) (cons (+ (car lst) pos) acc))))

; (b) iteration
(defun pos+i (lst)
  (let ((out nil)
        (pos 0))
    (dolist (obj lst)
      (progn (setf out (cons (+ obj pos) out))
             (setf pos (+ pos 1))))
    (reverse out)))

; (c) using mapcar
(defun pos+m (lst)
  (mapcar #'+ lst (pos+m-make-range 0 (length lst) 1 nil)))

(defun pos+m-make-range (cur max step acc)
  (if (> cur max)
      (reverse acc)
      (pos+m-make-range (+ cur step) max step (cons cur acc))))

; 6. After years of deliberation, a government commission has decided that
; lists should be represented by using the cdr to point to the first element
; and the car to point to the rest of the list. Define the government versions
; of the following functions:
;
; (a) cons
; (b) list
; (c) length (for lists)
; (d) member (for lists; no keywords)

; 7. Modify the program in Figure 3.6 to use fewer cons cells. (Hint: Use
; dotted lists.)
(defun rle-encode (lst)
  (reverse (rle-lib-encode lst nil)))

(defun rle-lib-encode (lst acc)
  (if (null lst)
      acc
      (let ((curr (car lst))
            (prev (car (car acc)))
            (cnt  (cdr (car acc))))
        (if (equal curr prev)
            (rle-lib-encode (cdr lst) (cons (cons curr (+ cnt 1)) (cdr acc)))
            (rle-lib-encode (cdr lst) (cons (cons curr 1) acc))))))

; 8. Define a function that takes a list and prints it in dot notation:
; > (showdots '(a b c))
; (A . (B . (C . NIL)))
; NIL

; This is only works for strings and is too hacky for real solution to the
; given exercise.
(defun showdots-string (lst)
  (showdots-string-lib (reverse lst) nil (length lst)))

(defun showdots-string-lib (lst acc len)
  (if (null lst)
      (concatenate 'string (reverse acc) (showdots-string-get-chars ")" len nil))
      (let ((acc_print (if (null acc) "LIN" acc)))
          (showdots-string-lib (cdr lst) (concatenate 'string acc_print "." (car lst) "(") len))))

(defun showdots-string-get-chars (chr n acc)
  (if (eql n 0)
    acc
    (showdots-string-get-chars chr (- n 1) (concatenate 'string acc chr))))

; 9. Write a program to find the longest finite path through a network
; represented as in Section 3.15. The network may contain cycles.
