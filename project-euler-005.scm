;
; https://projecteuler.net/problem=5
;
; Problem 5 - Smallest multiple
;
;     2520 is the smallest number that can be divided by each of the numbers
;     from 1 to 10 without any remainder.
;
;     What is the smallest positive number that is evenly divisible by all of
;     the numbers from 1 to 20?
;

(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (smallest-multiple (iota 10 1)))
    (print (smallest-multiple (iota 20 1)))))

(define unit-test (lambda ()
    (test-start "smallest-multiple")
    (test* "#" 1 (smallest-multiple '(1)))
    (test* "#" 2 (smallest-multiple '(1 2)))
    (test* "#" 6 (smallest-multiple '(1 2 3)))
    (test* "#" 12 (smallest-multiple '(1 2 3 4)))
    (test* "#" 30 (smallest-multiple '(1 2 3 4 5)))
    (test-end)))

; divisors に含まれる全ての数で割り切れる最小の数を求める.
(define smallest-multiple (lambda (divisors)
    (fold least-common-multiple 1 divisors)))

; 最大公約数を求める.
(define greatest-common-divisor (lambda (x1 x2)
    (cond
        ((> x1 x2)
            (greatest-common-divisor x2 x1))
        ((= (modulo x2 x1) 0)
            x1)
        (else
            (greatest-common-divisor x2 (modulo x2 x1))))))

; 最小公倍数を求める.
(define least-common-multiple (lambda (x1 x2)
    (/ (* x1 x2) (greatest-common-divisor x1 x2))))

