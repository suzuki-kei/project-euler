;
; https://projecteuler.net/problem=4
;
; Problem 4 - Largest palindrome product
;
;     A palindromic number reads the same both ways. The largest palindrome made
;     from the product of two 2-digit numbers is 9009 = 91 × 99.
;
;     Find the largest palindrome made from the product of two 3-digit numbers.
;

(use srfi-13)
(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (print (largest-palindrome-product 10 99))
    (print (largest-palindrome-product 100 999))))

(define unit-test (lambda ()
    (test-sequence)
    (test-palindrome-number?)
    (test-largest-palindrome-product)))

(define test-sequence (lambda ()
    (test-start "sequence")
    (test* "#1" '() (sequence 1 -1))
    (test* "#2" '() (sequence 1 0))
    (test* "#3" '(1) (sequence 1 1))
    (test* "#4" '(1 2) (sequence 1 2))
    (test* "#5" '(1 2 3) (sequence 1 3))
    (test* "#6" '(4 5 6) (sequence 4 6))
    (test-end)))

(define test-palindrome-number? (lambda ()
    (test-start "palindrome-number?")
    (test* "call by -1" #f (palindrome-number? -1))
    (test* "call by 0" #t (palindrome-number? 0))
    (test* "call by 1" #t (palindrome-number? 1))
    (test* "call by 2" #t (palindrome-number? 2))
    (test* "call by 3" #t (palindrome-number? 3))
    (test* "call by 10" #f (palindrome-number? 10))
    (test* "call by 11" #t (palindrome-number? 11))
    (test* "call by 12" #f (palindrome-number? 12))
    (test* "call by 100" #f (palindrome-number? 100))
    (test* "call by 101" #t (palindrome-number? 101))
    (test* "call by 102" #f (palindrome-number? 102))
    (test* "call by 12321" #t (palindrome-number? 12321))
    (test-end)))

(define test-largest-palindrome-product (lambda ()
    (test-start "largest-palindrome-product")
    (test* "call by 10 and 99" 9009 (largest-palindrome-product 10 99))
    (test-end)))

(define largest-palindrome-product (lambda (lower upper)
    ; lower 以上 upper 以下の数同士を掛けた値の重複の無いリスト.
    (define uniqued-products
        (fold
            append
            '()
            (map
                (lambda (lhs)
                    (map
                        (lambda (rhs) (* lhs rhs))
                        (sequence lhs upper)))
                (sequence lower upper))))
    ; uniqued-products のうち回文数になる値のリスト.
    (define palindrome-numbers
        (filter
            palindrome-number?
            uniqued-products))
    ; palindrome-numbers の最大値.
    (car
        (sort palindrome-numbers >=))))

; lower 以上 upper 以下の整数からなるリストを生成する.
; lower が upper より大きい場合は空のリストを生成する.
(define sequence (lambda (lower upper)
    (if (> lower upper) '()
        (iota (+ (- upper lower) 1) lower))))

; x が回文数であるか判定する.
(define palindrome-number? (lambda (x)
    (if (< x 0) #f
        (let ((digits (split-to-digits x)))
            (equal? digits (reverse digits))))))

(define split-to-digits (lambda (x)
    (define split-to-digits (lambda (x digits)
        (if (= x 0) (reverse digits)
            (split-to-digits (div x 10) (cons (modulo x 10) digits)))))
    (split-to-digits x '())))

