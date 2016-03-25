; ------------- chapter 4 exercises
;
; 1. Define a function to take a square array (an array whose dimensions are
; (n n)) and rotate it 90 degrees clockwise.
; > (quarter-turn #2A((a b) (c d)))
; #2A((C A) (D B))
(defun q-turn (arr)
  "Rotate any 2-dimensional array clockwise quarter turn"
  (q-turn-raw arr 0 (make-array (list (array-dimension arr 1)
                                      (array-dimension arr 0)))))

(defun q-turn-raw (arr col acc)
  (if (eql col (array-dimension arr 1))
      acc
      (q-turn-raw arr (+ col 1) (q-turn-raw-col arr
                                                col
                                                (- (array-dimension acc 1) 1)
                                                acc))))

(defun q-turn-raw-col (arr col cur-row acc)
  (if (eql cur-row -1)
      acc
      (progn (setf (aref acc col (- (array-dimension acc 1) cur-row 1))
                   (aref arr cur-row col))
             (q-turn-raw-col arr col (- cur-row 1) acc))))

; 2. Read the description of reduce on page 368, then use it to define:
; (a) copy-list

(defun ch4-list-copy (lst)
  (reduce #'(lambda (el acc) (cons el acc)) lst :from-end t :initial-value nil))

; (b) reverse (for lists)
(defun ch4-list-reverse (lst)
  (reduce #'(lambda (acc el) (cons el acc)) lst :initial-value nil))

; 3. Define a structure to represent a tree where each node contains some data
; and has up to three children. Define
; (a) a function to copy such a tree (so that no node in the copy is eql to a
;     node in the original)
; (b) a function that takes an object and such a tree, and returns true if
;     object is eql to the data field of one of the nodes
(defstruct tree3 val a b c)

; 4. Define a function that take a BST and returns a list of its elements from
; greatest to least.

; 5. Define bst-adjoin. This function should take the same arguments as
; bst-insert, but should only insert the object if there is nothing eql to it
; in the tree.

; 6. The contents of any hash table can be described by an assoc-list whose
; elements are (k . v), for each key value pair in the hash table. Define a
; function that
; (a) takes an assoc-list and returns a corresponding hash table
(defun al-to-ht (al)
  (reduce #'(lambda (acc el)
              (progn (push (cdr el) (gethash (car el) acc))
                     acc))
          al
          :initial-value (make-hash-table)))

; (b) takes a hash table and returns a corresponding assoc-list
(defun ht-to-al (ht)
  (let ((al nil))
    (maphash #'(lambda (k v) (setf al (cons (cons k v) al))) ht)
    al))
