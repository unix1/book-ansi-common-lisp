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

;(defun pos+ (lst)
