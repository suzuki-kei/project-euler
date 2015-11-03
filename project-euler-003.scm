;
; https://projecteuler.net/problem=3
;
; Problem 3 - Largest prime factor
;
;     The prime factors of 13195 are 5, 7, 13 and 29.
;     What is the largest prime factor of the number 600851475143 ?
;

(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (largest-prime-factor 600851475143))))

(define unit-test (lambda ()
    (test-start "factorize")
    (test* "call by -1" #f (factorize -1))
    (test* "call by 0" #f (factorize 0))
    (test* "call by 1" '(1) (factorize 1))
    (test* "call by 2" '(2) (factorize 2))
    (test* "call by 3" '(3) (factorize 3))
    (test* "call by 4" '(2 2) (factorize 4))
    (test* "call by 5" '(5) (factorize 5))
    (test* "call by 6" '(3 2) (factorize 6))
    (test* "call by 7" '(7) (factorize 7))
    (test* "call by 8" '(2 2 2) (factorize 8))
    (test* "call by 9" '(3 3) (factorize 9))
    (test* "call by 10" '(5 2) (factorize 10))
    (test-end)
    (test-start "largest-prime-factor")
    (test* "call by 2" 2 (largest-prime-factor 2))
    (test* "call by 3" 3 (largest-prime-factor 3))
    (test* "call by 4" 2 (largest-prime-factor 4))
    (test* "call by 5" 5 (largest-prime-factor 5))
    (test* "call by 6" 3 (largest-prime-factor 6))
    (test* "call by 7" 7 (largest-prime-factor 7))
    (test* "call by 8" 2 (largest-prime-factor 8))
    (test* "call by 9" 3 (largest-prime-factor 9))
    (test* "call by 10" 5 (largest-prime-factor 10))
    (test* "call by 11" 11 (largest-prime-factor 11))
    (test-end)))

(define largest-prime-factor (lambda (x)
    (car (sort (factorize x) >=))))

; x を素因数分解する.
(define factorize (lambda (x)
    (define factorize (lambda (x divisor factors)
        (cond
            ((<= x 1)
                factors)
            ((= (modulo x divisor) 0)
                (factorize (/ x divisor) divisor (cons divisor factors)))
            (else
                (factorize x (+ divisor 1) factors)))))
    (cond
        ((< x 1)
            #f)
        ((= x 1)
            '(1))
        ((> x 1)
            (factorize x 2 '()))
        (else
            #f))))

