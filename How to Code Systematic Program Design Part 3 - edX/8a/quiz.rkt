(require 2htdp/image)
;  PROBLEM 1:

;; (X (listof X) -> Y) Z (listof X) -> Y
(define (arrange-all c b i)
  (cond [(empty? i) b]
        [else
         (c (first i)
            (arrange-all c b (rest i)))]))

(check-expect (above-all2 empty) empty-image)
(check-expect (above-all2 (list (rectangle 20 40 "solid" "red") (star 30 "solid" "yellow")))
              (above (rectangle 20 40 "solid" "red") (star 30 "solid" "yellow")))
(check-expect (above-all2 (list (circle 30 "outline" "black") (circle 50 "outline" "black") (circle 70 "outline" "black")))
              (above (circle 30 "outline" "black") (circle 50 "outline" "black") (circle 70 "outline" "black")))

;; (listof Image) -> Image
(define (above-all2 i)
  (arrange-all above empty-image i))


(check-expect (beside-all2 empty) (rectangle 0 0 "solid" "white"))
(check-expect (beside-all2 (list (rectangle 50 40 "solid" "blue") (triangle 30 "solid" "pink")))
              (beside (rectangle 50 40 "solid" "blue") (triangle 30 "solid" "pink")))
(check-expect (beside-all2 (list (circle 10 "outline" "red") (circle 20 "outline" "blue") (circle 10 "outline" "yellow")))
              (beside (circle 10 "outline" "red") (circle 20 "outline" "blue") (circle 10 "outline" "yellow")))


;; (listof Image) -> Image
(define (beside-all2 i)
  (arrange-all beside (rectangle 0 0 "solid" "white") i))


;  PROBLEM 2:


;; Function 1
;; ==========

;; (listof String) -> (listof Natural)
;; produces a list of the lengths of each string in los
(check-expect (lengths empty) empty)
(check-expect (lengths (list "apple" "banana" "pear")) (list 5 6 4))

; (define (lengths lst) empty)
(define (lengths lst)
  (map string-length lst))



;; Function 2
;; ==========

;; (listof Natural) -> (listof Natural)
;; produces a list of just the odd elements of lon
(check-expect (odd-only empty) empty)
(check-expect (odd-only (list 1 2 3 4 5)) (list 1 3 5))

; (define (odd-only lon) empty)
(define (odd-only lon)
  (filter odd? lon))


;; Function 3
;; ==========

;; (listof Natural -> Boolean
;; produce true if all elements of the list are odd
(check-expect (all-odd? empty) true)
(check-expect (all-odd? (list 1 2 3 4 5)) false)
(check-expect (all-odd? (list 5 5 79 13)) true)

;(define (all-odd? lon) empty)
(define (all-odd? lon)
  (andmap odd? lon))


;; Function 4
;; ==========

;; (listof Natural) -> (listof Natural)
;; subtracts n from each element of the list
(check-expect (minus-n empty 5) empty)
(check-expect (minus-n (list 4 5 6) 1) (list 3 4 5))
(check-expect (minus-n (list 10 5 7) 4) (list 6 1 3))

;(define (minus-n lon n) empty)

(define (minus-n lon n)
  (local [(define (-n i) (- i n))]
    (map -n lon)))



;  PROBLEM 3

; (String X Z -> Y) (Y Z -> Z) X X X X X Z Region -> Y

(define (fold-region c1 c2 c3 b1 b2 b3 b4 b5 b6 r)
  (local [(define (fn-for-region r)
            (c1 (region-name r)
                (fn-for-type (region-type r))
                (fn-for-sub (region-subregions r))))
          (define (fn-for-type t)
            (cond [(string=? t "Continent") b1]
                  [(string=? t "Country") b2]
                  [(string=? t "Province") b3]
                  [(string=? t "State") b4]
                  [(string=? t "City") b5]))
          (define (fn-for-sub s)
            (cond [(empty? s) b6]
                  [else
                   (c3 (fn-for-region (first s))
                       (fn-for-sub (rest s)))]))]
    (fn-for-region r)))

          



                   
