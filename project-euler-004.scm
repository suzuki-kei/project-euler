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
    (print (largest-palindrome-product 10 99))
    (print (largest-palindrome-product 100 999))))

(define unit-test (lambda ()
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
(define sequence (lambda (lower upper)
    (iota (+ (- upper lower) 1) lower)))

; x が回文数であるか判定する.
(define palindrome-number? (lambda (x)
    (let ((string (number->string x)))
        (equal? string (string-reverse string)))))
