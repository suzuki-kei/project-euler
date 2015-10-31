;
; https://projecteuler.net/problem=3
;
; Problem 3 - Largest prime factor
;
;     The prime factors of 13195 are 5, 7, 13 and 29.
;     What is the largest prime factor of the number 600851475143 ?
;

(define main (lambda (arguments)
    (print (car (factorize 600851475143)))))

(define factorize (lambda (x)
    (define factorize (lambda (x divisor factors)
        (cond
            ((< x divisor)
                factors)
            ((= (modulo x divisor) 0)
                (factorize (/ x divisor) divisor (cons divisor factors)))
            (else
                (factorize x (+ divisor 1) factors)))))
    (factorize x 2 '())))

