(defun rle-encode (lst)
  (reverse (rle-lib-encode lst nil)))

(defun rle-lib-encode (lst acc)
  (if (null lst)
      acc
      (let ((curr (car lst))
            (prev (car acc))
            (cnt  (car (cdr acc))))
        (if (equal curr prev)
            (rle-lib-encode (cdr lst) (cons curr (cons (+ cnt 1) (cdr (cdr acc)))))
            (rle-lib-encode (cdr lst) (cons curr (cons 1 acc)))))))

(defun rle-decode (lst)
  (reverse (rle-lib-decode lst nil)))

(defun rle-lib-decode (lst acc)
  (if (null lst)
      acc
      (let ((cnt (car lst))
            (chr (car (cdr lst))))
        (rle-lib-decode (cdr (cdr lst)) (rle-lib-seq chr cnt acc)))))

(defun rle-lib-seq (chr cnt acc)
  (if (eql cnt 0)
      acc
      (rle-lib-seq chr (- cnt 1) (cons chr acc))))
