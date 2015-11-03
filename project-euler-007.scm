;
; https://projecteuler.net/problem=7
;
; Problem 7 - 10001st prime
;
;     By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;     that the 6th prime is 13.
;
;     What is the 10001st prime number?
;

(use srfi-1)
(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (nth-prime 6))
    (print (nth-prime 10001))))

(define unit-test (lambda ()
    (test-start "sequence")
    (test* "#1" '() (sequence 1 -1))
    (test* "#2" '() (sequence 1 0))
    (test* "#3" '(1) (sequence 1 1))
    (test* "#4" '(1 2) (sequence 1 2))
    (test* "#5" '(1 2 3) (sequence 1 3))
    (test* "#6" '(4 5 6) (sequence 4 6))
    (test-end)
    (test-start "prime?")
    (test* "call by -1" #f (prime? -1))
    (test* "call by 0" #f (prime? 0))
    (test* "call by 1" #f (prime? 1))
    (test* "call by 2" #t (prime? 2))
    (test* "call by 3" #t (prime? 3))
    (test* "call by 4" #f (prime? 4))
    (test* "call by 5" #t (prime? 5))
    (test* "call by 6" #f (prime? 6))
    (test* "call by 7" #t (prime? 7))
    (test* "call by 8" #f (prime? 8))
    (test* "call by 9" #f (prime? 9))
    (test* "call by 10" #f (prime? 10))
    (test* "call by 11" #t (prime? 11))
    (test-end)))

; n 番目までの素数を求める.
(define nth-prime (lambda (n)
    (define primes (lambda (x leading-primes)
        (cond
            ((> (+ (length leading-primes) 1) n)
                leading-primes)
            ((prime? x)
                (primes (+ x 1) (cons x leading-primes)))
            (else
                (primes (+ x 1) leading-primes)))))
    (car (primes 2 '()))))

(define sequence (lambda (lower upper)
    (if (> lower upper) '()
        (iota (+ (- upper lower) 1) lower))))

(define prime? (lambda (x)
    (define prime? (lambda (x divisor)
        (cond
            ((<= x 1)
                #f)
            ((<= x divisor)
                #t)
            ((= (modulo x divisor) 0)
                #f)
            (else
                (prime? x (+ divisor 1))))))
    (prime? x 2)))

