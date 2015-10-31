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

(define main (lambda (arguments)
    (print (sum-of-multiples 1000))))

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

