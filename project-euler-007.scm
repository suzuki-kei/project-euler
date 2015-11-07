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
(use srfi-43)
(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (nth-prime 6))
    (print (nth-prime 10001))))

(define unit-test (lambda ()
    (test-sequence)
    (test-make-eratosthenes-sieve)
    (test-generate-primes)
    (test-nth-prime)))

(define test-sequence (lambda ()
    (test-start "sequence")
    (test* "#1" '() (sequence 1 -1))
    (test* "#2" '() (sequence 1 0))
    (test* "#3" '(1) (sequence 1 1))
    (test* "#4" '(1 2) (sequence 1 2))
    (test* "#5" '(1 2 3) (sequence 1 3))
    (test* "#6" '(4 5 6) (sequence 4 6))
    (test-end)))

(define test-make-eratosthenes-sieve (lambda ()
    (test-start "make-eratosthenes-sieve")
    (test* "call by -1" #() (make-eratosthenes-sieve -1))
    (test* "call by 0" #(#f) (make-eratosthenes-sieve 0))
    (test* "call by 1" #(#f #f) (make-eratosthenes-sieve 1))
    (test* "call by 2" #(#f #f #t) (make-eratosthenes-sieve 2))
    (test* "call by 3" #(#f #f #t #t) (make-eratosthenes-sieve 3))
    (test* "call by 4" #(#f #f #t #t #f) (make-eratosthenes-sieve 4))
    (test* "call by 5" #(#f #f #t #t #f #t) (make-eratosthenes-sieve 5))
    (test* "call by 6" #(#f #f #t #t #f #t #f) (make-eratosthenes-sieve 6))
    (test* "call by 7" #(#f #f #t #t #f #t #f #t) (make-eratosthenes-sieve 7))
    (test* "call by 8" #(#f #f #t #t #f #t #f #t #f) (make-eratosthenes-sieve 8))
    (test* "call by 9" #(#f #f #t #t #f #t #f #t #f #f) (make-eratosthenes-sieve 9))
    (test* "call by 10" #(#f #f #t #t #f #t #f #t #f #f #f) (make-eratosthenes-sieve 10))
    (test* "call by 11" #(#f #f #t #t #f #t #f #t #f #f #f #t) (make-eratosthenes-sieve 11))
    (test-end)))

(define test-generate-primes (lambda ()
    (test-start "generate-primes")
    (test* "call by -1" #() (generate-primes -1))
    (test* "call by 0" #() (generate-primes 0))
    (test* "call by 1" #() (generate-primes 1))
    (test* "call by 2" #(2) (generate-primes 2))
    (test* "call by 3" #(2 3) (generate-primes 3))
    (test* "call by 4" #(2 3) (generate-primes 4))
    (test* "call by 5" #(2 3 5) (generate-primes 5))
    (test* "call by 6" #(2 3 5) (generate-primes 6))
    (test* "call by 7" #(2 3 5 7) (generate-primes 7))
    (test* "call by 8" #(2 3 5 7) (generate-primes 8))
    (test* "call by 9" #(2 3 5 7) (generate-primes 9))
    (test* "call by 10" #(2 3 5 7) (generate-primes 10))
    (test* "call by 11" #(2 3 5 7 11) (generate-primes 11))
    (test-end)))

(define test-nth-prime (lambda ()
    (test-start "nth-prime-procedure")
    (test* "call by 1" 2 (nth-prime 1))
    (test* "call by 2" 3 (nth-prime 2))
    (test* "call by 3" 5 (nth-prime 3))
    (test* "call by 4" 7 (nth-prime 4))
    (test* "call by 5" 11 (nth-prime 5))
    (test* "call by 6" 13 (nth-prime 6))
    (test-end)))

; lower 以上 upper 以下の整数からなるリストを生成する.
; lower が upper より大きい場合は空のリストを生成する.
(define sequence (lambda (lower upper)
    (if (> lower upper) '()
        (iota (+ (- upper lower) 1) lower))))

; n 以下の素数からなる vector を生成する.
(define generate-primes (lambda (n)
    (define generate-primes (lambda (table table-index sieve sieve-index)
        (cond
            ((>= table-index (vector-length table))
                table)
            ((>= sieve-index (vector-length sieve))
                table)
            ((vector-ref sieve sieve-index)
                (vector-set! table table-index sieve-index)
                (generate-primes table (+ table-index 1) sieve (+ sieve-index 1)))
            (else
                (generate-primes table table-index sieve (+ sieve-index 1))))))
    (let* ((sieve (make-eratosthenes-sieve n))
           (table (make-vector (vector-count (lambda (i x) x) sieve))))
        (generate-primes table 0 sieve 0))))

; 0 以上 n 以下の整数の素数判定を行うためのエラトステネスの篩を生成する.
(define make-eratosthenes-sieve (lambda (n)
    (define sieve-multiples (lambda (table base multiple)
        (cond
            ((>= multiple (vector-length table))
                table)
            ((not (vector-ref table base))
                table)
            (else
                (begin
                    (vector-set! table multiple #f)
                    (sieve-multiples table base (+ multiple base)))))))
    (let ((table (make-vector (max (+ n 1) 0) #t)))
        (if (> (vector-length table) 0) (vector-set! table 0 #f))
        (if (> (vector-length table) 1) (vector-set! table 1 #f))
            (fold
                (lambda (base table) (sieve-multiples table base (* base 2)))
                table
                (sequence 2 n)))))

; n 番目の素数を求める.
(define nth-prime
    (let ((primes (generate-primes 104743)))
        (lambda (n) (vector-ref primes (- n 1)))))

