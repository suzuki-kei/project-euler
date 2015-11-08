;
; https://projecteuler.net/problem=8
;
; Problem 8 - Largest product in a series
;
;     The four adjacent digits in the 1000-digit number that have the greatest
;     product are 9 × 9 × 8 × 9 = 5832.
;
;         73167176531330624919225119674426574742355349194934
;         96983520312774506326239578318016984801869478851843
;         85861560789112949495459501737958331952853208805511
;         12540698747158523863050715693290963295227443043557
;         66896648950445244523161731856403098711121722383113
;         62229893423380308135336276614282806444486645238749
;         30358907296290491560440772390713810515859307960866
;         70172427121883998797908792274921901699720888093776
;         65727333001053367881220235421809751254540594752243
;         52584907711670556013604839586446706324415722155397
;         53697817977846174064955149290862569321978468622482
;         83972241375657056057490261407972968652414535100474
;         82166370484403199890008895243450658541227588666881
;         16427171479924442928230863465674813919123162824586
;         17866458359124566529476545682848912883142607690042
;         24219022671055626321111109370544217506941658960408
;         07198403850962455444362981230987879927244284909188
;         84580156166097919133875499200524063689912560717606
;         05886116467109405077541002256983155200055935729725
;         71636269561882670428252483600823257530420752963450
;
;     Find the thirteen adjacent digits in the 1000-digit number that have the
;     greatest product. What is the value of this product?
;

(use gauche.test)

(define main (lambda (arguments)
    (unit-test)
    (receive (series product) (find-largest-product-series digits 4) (print product " " series))
    (receive (series product) (find-largest-product-series digits 13) (print product " " series))))

(define unit-test (lambda ()
    (test-pick-series)))

(define test-pick-series (lambda ()
    (test-start "pick-series")
    (test* "#1" '() (pick-series #(0 1 2 3 4 5 6 7 8 9) 0 0))
    (test* "#2" '(0) (pick-series #(0 1 2 3 4 5 6 7 8 9) 0 1))
    (test* "#3" '(0 1) (pick-series #(0 1 2 3 4 5 6 7 8 9) 0 2))
    (test* "#4" '(0 1 2) (pick-series #(0 1 2 3 4 5 6 7 8 9) 0 3))
    (test* "#5" '(1 2 3) (pick-series #(0 1 2 3 4 5 6 7 8 9) 1 3))
    (test* "#6" '(2 3 4) (pick-series #(0 1 2 3 4 5 6 7 8 9) 2 3))
    (test* "#7" '(3 4 5) (pick-series #(0 1 2 3 4 5 6 7 8 9) 3 3))
    (test* "#8" '(7 8 9) (pick-series #(0 1 2 3 4 5 6 7 8 9) 7 3))
    (test* "#9" '(8 9) (pick-series #(0 1 2 3 4 5 6 7 8 9) 8 2))
    (test* "#10" '(9) (pick-series #(0 1 2 3 4 5 6 7 8 9) 9 1))
    (test-end)))

; digits の中で連続する run-length 個の数値を掛け合わせた値が最大のものを求め, 
; (values 数値のリスト 掛け合わせた値) である多値を返す.
(define find-largest-product-series (lambda (digits run-length)
    (define find-largest-product-series (lambda (current-index largest-series largest-product)
        (if (> (+ current-index run-length) (vector-length digits))
            (values largest-series largest-product)
            (let* ((current-series (pick-series digits current-index run-length))
                   (current-product (fold * 1 current-series)))
                (if (> current-product largest-product)
                    (find-largest-product-series (+ current-index 1) current-series current-product)
                    (find-largest-product-series (+ current-index 1) largest-series largest-product))))))
    (find-largest-product-series 0 #f 0)))

; digits の start-index から length 個を取り出す.
(define pick-series (lambda (digits start-index length)
    (define pick-series (lambda (digits i length)
        (if (<= length 0) '()
            (cons
                (vector-ref digits i)
                (pick-series digits (+ i 1) (- length 1))))))
    (pick-series digits start-index length)))

(define digits
    (list->vector
        (map
            digit->integer
            (filter
                char-numeric?
                (string->list "73167176531330624919225119674426574742355349194934
                               96983520312774506326239578318016984801869478851843
                               85861560789112949495459501737958331952853208805511
                               12540698747158523863050715693290963295227443043557
                               66896648950445244523161731856403098711121722383113
                               62229893423380308135336276614282806444486645238749
                               30358907296290491560440772390713810515859307960866
                               70172427121883998797908792274921901699720888093776
                               65727333001053367881220235421809751254540594752243
                               52584907711670556013604839586446706324415722155397
                               53697817977846174064955149290862569321978468622482
                               83972241375657056057490261407972968652414535100474
                               82166370484403199890008895243450658541227588666881
                               16427171479924442928230863465674813919123162824586
                               17866458359124566529476545682848912883142607690042
                               24219022671055626321111109370544217506941658960408
                               07198403850962455444362981230987879927244284909188
                               84580156166097919133875499200524063689912560717606
                               05886116467109405077541002256983155200055935729725
                               71636269561882670428252483600823257530420752963450")))))

