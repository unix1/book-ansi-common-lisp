; ------------- chapter 9 exercises
;
; 1. Define a function that takes a list of reals and returns true iff they are
; in nondecreasing order.

; 2. Define a function that takes an integer number of cents and returns four
; values showing how to make that number out of 25-, 10-, 5- and 1-cent pieces,
; using the smallest total number of coins.

; 3. A faraway planet is inhabited by two kinds of beings, wigglies and
; wobblies. Wigglies and wobblies are equally good at singing. Every year there
; is a great competition to choose the ten best singers. Here are the results
; for the past ten years:
;
; +----------+---+---+---+---+---+---+---+---+---+---+
; | YEAR     | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |10 |
; +----------+---+---+---+---+---+---+---+---+---+---+
; | WIGGLIES | 6 | 5 | 6 | 4 | 5 | 5 | 4 | 5 | 6 | 5 |
; +----------+---+---+---+---+---+---+---+---+---+---+
; | WOBBLIES | 4 | 5 | 4 | 6 | 5 | 5 | 6 | 5 | 4 | 5 |
; +----------+---+---+---+---+---+---+---+---+---+---+
;
; Write a program to simulate such a contest. Do your results suggest that the
; committee is, in fact, choosing the ten best singers each year?

; 4. Define a function that takes 8 reals representing the endpoints of two
; segments in 2-space, and returns either nil if the segments do not intersect,
; or two values representing the x- and y-coordinates of the intersection if
; they do.

; 5. Suppose f is a function of one (real) argument, and that min and max are
; nonzero reals with different signs such that f has a root (returns zero) for
; one argument i such that min < i < max. Define a function that takes four
; arguments, f, min, max, and epsilon, and returns an approximation of i
; accurate to within plus or minus epsilon.

; 6. Homer's method is a trick for evaluating polynomials efficiently. To find
; ax^3+bx^2+cx+d you evaluate x(x(ax+b)+c)+d. Define a function that takes one
; or more arguments - the value of x followed by n reals representing the
; coefficients of an (n-1)th-degree polynomial - and calculates the value of
; the polynomial by Homer's method.

; 7. How many bits would you estimate your implementation uses to represent
; fixnums?

; 8. How many distinct types of floats does your implementation provide?
