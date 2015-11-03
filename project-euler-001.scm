;
; https://projecteuler.net/problem=1
;
; Problem 1 - Multiples of 3 and 5
;
;     If we list all the natural numbers below 10 that are multiples of 3 or 5,
;     we get 3, 5, 6 and 9. The sum of these multiples is 23.
;
;     Find the sum of all the multiples of 3 or 5 below 1000.
;

(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (sum-of-multiples 1000))))

(define unit-test (lambda ()
    (test-sum-of-multiples)))

(define test-sum-of-multiples (lambda ()
    (test-start "sum-of-multiples")
    (test* "call by 0" 0 (sum-of-multiples 0))
    (test* "call by 1" 0 (sum-of-multiples 1))
    (test* "call by 2" 0 (sum-of-multiples 2))
    (test* "call by 3" 0 (sum-of-multiples 3))
    (test* "call by 4" 3 (sum-of-multiples 4))
    (test* "call by 5" 3 (sum-of-multiples 5))
    (test* "call by 6" 8 (sum-of-multiples 6))
    (test* "call by 7" 14 (sum-of-multiples 7))
    (test* "call by 8" 14 (sum-of-multiples 8))
    (test* "call by 9" 14 (sum-of-multiples 9))
    (test* "call by 10" 23 (sum-of-multiples 10))
    (test* "call by 11" 33 (sum-of-multiples 11))
    (test-end)))

(define sum-of-multiples (lambda (x)
    (define sum-of-multiples (lambda (x multiple-of? sum)
        (cond
            ((<= x 0)
                sum)
            ((multiple-of? x)
                (sum-of-multiples (- x 1) multiple-of? (+ sum x)))
            (else
                (sum-of-multiples (- x 1) multiple-of? sum)))))
    (define multiple-of-3-or-5? (lambda (x)
        (or
            (= (modulo x 3) 0)
            (= (modulo x 5) 0))))
    (sum-of-multiples (- x 1) multiple-of-3-or-5? 0)))

