(defmacro cah (lst)
  `(car ,lst))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
      (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                    choices)))))

(defmacro in-list (obj lst)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
      (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                    lst)))))
