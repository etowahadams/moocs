; P2

(require 2htdp/image)

;; decreasing-image-starter.rkt

;; ================
;; Constants

(define TEXT-COLOR "black")
(define TEXT-SIZE 20)
(define SPACE (square 5 "solid" "white"))

;; =================
;; Functions

;; Natural -> Image
;; consumes a Natural n and produces an image of all the numbers from n to 0 side by side.
(check-expect (decreasing-image 0) (text "0" TEXT-SIZE TEXT-COLOR))
(check-expect (decreasing-image 2) (beside (text "2" TEXT-SIZE TEXT-COLOR)
                                           SPACE
                                           (text "1" TEXT-SIZE TEXT-COLOR)
                                           SPACE
                                           (text "0" TEXT-SIZE TEXT-COLOR)))

;(define (decreasing-image n) SPACE)   ;stub

(define (decreasing-image n)
  (cond [(zero? n) (text "0" TEXT-SIZE TEXT-COLOR)]
        [else
         (beside (text (number->string n) TEXT-SIZE TEXT-COLOR)
                 SPACE
                 (decreasing-image (- n 1)))]))


; P3

;; ===================
;; Data Definitions

;; ListOfNatural is one of:
;; - empty
;; - (cons Natural ListOfNatural)
;; interp. a list of odd numbers

(define L1 empty)
(define L2 (cons 5 empty))
(define L3 (cons 3 (cons 5 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound data: (cons Natural ListOfOdd)
;; - self-reference: (rest lon) is ListOfOdd

;; ==================
;; Functions

;; Natural -> ListOfNatural
;; consumes a natural number n, and produces a list of all 
;; the odd numbers from n down to 1

(check-expect (odd-from-n 0) empty)
(check-expect (odd-from-n 1) (cons 1 empty))
(check-expect (odd-from-n 2) (cons 1 empty))
(check-expect (odd-from-n 5) (cons 5 (cons 3 (cons 1 empty))))

;(define (odd-from-n n) empty)   ;stub

(define (odd-from-n n)
  (cond [(zero? n) empty]
        [(odd? n) (cons n (odd-from-n (sub1 n)))]
        [else (odd-from-n (sub1 n))]))