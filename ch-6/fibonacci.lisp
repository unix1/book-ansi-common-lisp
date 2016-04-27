(defun fibonacci (n)
  (fibonacci-raw 1 1 1 n))

(defun fibonacci-raw (p1 p2 cur n)
  (cond ((eql n 0) 1)
        ((eql n 1) 1)
        ((eql cur n) p2)
        (t (fibonacci-raw p2 (+ p1 p2) (+ cur 1) n))))
