; ------------- chapter 7 exercises
;
; 1. Define a function that takes a filename and returns a list of strings
; representing each line in the file.
(defun file-lines (filename)
  (with-open-file (str filename :direction :input)
    (stream-lines str)))

(defun stream-lines (str &optional acc)
  (let ((line (read-line str nil `eof)))
    (if (eql line `eof)
      acc
      (stream-lines str (cons line acc)))))

; 2. Define a function that takes a filename and returns a list of the
; expressions in the file.
(defun file-expressions (filename)
  (with-open-file (str filename :direction :input)
    (stream-expressions str)))

(defun stream-expressions (str &optional acc)
  (let ((expr (read str nil `eof)))
    (if (eql expr `eof)
      acc
      (stream-expressions str (cons expr acc)))))

; 3. Suppose that in some format for text files, comments are indicated by a %
; character. Everything from this character to the end of the line is ignored.
; Define a function that takes two filenames, and writes to the second file a
; copy of the first, minus comments.
(defun strip-comments (file-from file-to chr)
  (with-open-file (in file-from :direction :input)
    (with-open-file (out file-to :direction :output :if-exists :supersede)
      (strip-comments-stream in out chr))))

(defun strip-comments-stream (in out chr)
  (let ((line (read-line in nil :eof)))
    (if (eql line :eof)
      t
      (progn (strip-comments-stream-line line in out chr)
             (strip-comments-stream in out chr)))))

(defun strip-comments-stream-line (line in out chr &optional (pos 0))
  (if (eql pos (length line))
      (format out "~%")
      (let ((line-char (char line pos)))
        (if (eql line-char chr)
            (format out "~%")
            (progn (princ line-char out)
                   (strip-comments-stream-line line in out chr (+ pos 1)))))))

; 4. Define a function that takes a two-dimensimonal array of floats and
; displays it in neat columns. Each element should be printed with two digits
; after the decimal point, in a field of 10 characters wide. (Assume all will
; fit.) You will need array-dimensions (page 361).

; 5. Modify stream-subst to allow wildcards in the pattern. If the character +
; occurs in old, it should match any input character.

; 6. Modify stream-subst so that the pattern can include an element that
; matches any digit character, an element that matches any alphanumeric
; character, or an element that matches any character. The pattern must also
; be able to match any specific input character. (Hint: old can no longer be
; a string).
