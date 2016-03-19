; ------------- chapter 3 exercises
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
  (if (eql (print cur-row) -1)
      acc
      (progn (setf (aref acc col (- (array-dimension acc 1) cur-row 1))
                   (aref arr cur-row col))
             (q-turn-raw-col arr col (- cur-row 1) acc))))

; 2. Read the description of reduce on page 368, then use it to define:
; (a) copy-list

;(defun ch4-copy-list (lst)
;  (reduce #'lambda

; (b) reverse (for lists)

; 3. 
