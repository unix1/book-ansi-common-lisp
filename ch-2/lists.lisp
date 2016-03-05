; ------------ chapter 2 content

; non-tail recursive list length
(defun length-of-list (lst)
  (if (null lst)
    0
    (+ (length-of-list (cdr lst)) 1)))

; tail recursive list length
(defun length-of-list-tco (lst)
  (length-of-list-tco-lib lst 0))

(defun length-of-list-tco-lib (lst cnt)
  (if (null lst)
      cnt
      (length-of-list-tco-lib (cdr lst) (+ cnt 1))))
