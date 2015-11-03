;
; https://projecteuler.net/problem=6
;
; Problem 6 - Sum square difference
;
;     The sum of the squares of the first ten natural numbers is,
;
;         1^2 + 2^2 + ... + 10^2 = 385
;
;     The square of the sum of the first ten natural numbers is,
;
;         (1 + 2 + ... + 10)^2 = 55^2 = 3025
;
;     Hence the difference between the sum of the squares of the first ten
;     natural numbers and the square of the sum is 3025 − 385 = 2640.
;
;     Find the difference between the sum of the squares of the first one
;     hundred natural numbers and the square of the sum.
;

(use srfi-1)
(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (difference-between-sum-of-squares-and-squares-of-sum 10))
    (print (difference-between-sum-of-squares-and-squares-of-sum 100))))

(define unit-test (lambda ()
    (test-start "square")
    (test* "call by -1" 1 (square -1))
    (test* "call by 0" 0 (square 0))
    (test* "call by 1" 1 (square 1))
    (test* "call by 2" 4 (square 2))
    (test* "call by 3" 9 (square 3))
    (test-end)
    (test-start "sum")
    (test* "call by ()" 0 (sum '()))
    (test* "call by (0)" 0 (sum '(0)))
    (test* "call by (1)" 1 (sum '(1)))
    (test* "call by (1 2)" 3 (sum '(1 2)))
    (test* "call by (1 2 3)" 6 (sum '(1 2 3)))
    (test* "call by (-1 -2 -3)" -6 (sum '(-1 -2 -3)))
    (test-end)
    (test-start "sum-of-squares")
    (test* "call by -1" #f (sum-of-squares -1))
    (test* "call by 0" 0 (sum-of-squares 0))
    (test* "call by 1" 1 (sum-of-squares 1))
    (test* "call by 2" 5 (sum-of-squares 2))
    (test* "call by 3" 14 (sum-of-squares 3))
    (test* "call by 4" 30 (sum-of-squares 4))
    (test* "call by 5" 55 (sum-of-squares 5))
    (test-end)
    (test-start "square-of-sum")
    (test* "call by -1" #f (square-of-sum -1))
    (test* "call by 0" 0 (square-of-sum 0))
    (test* "call by 1" 1 (square-of-sum 1))
    (test* "call by 2" 9 (square-of-sum 2))
    (test* "call by 3" 36 (square-of-sum 3))
    (test* "call by 4" 100 (square-of-sum 4))
    (test* "call by 5" 225 (square-of-sum 5))
    (test-end)))

; n 以下の自然数の "二乗の総和" と "総和の二乗" の差を求める.
(define difference-between-sum-of-squares-and-squares-of-sum (lambda (n)
    (-
        (square-of-sum n)
        (sum-of-squares n))))

(define square (lambda (x)
    (* x x)))

(define sum (lambda (xs)
    (fold + 0 xs)))

; n 以下の自然数の "二乗の総和" を求める.
(define sum-of-squares (lambda (n)
    (if (< n 0) #f
        (sum (map square (iota n 1))))))

; n 以下の自然数の "総和の二乗" を求める.
(define square-of-sum (lambda (n)
    (if (< n 0) #f
        (square (sum (iota n 1))))))

