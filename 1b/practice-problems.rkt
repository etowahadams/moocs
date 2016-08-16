(require 2htdp/image)

;P2

;; String -> Boolean
;; Produces true if length of string is less than 5.
(check-expect (under-5 "red") true)
(check-expect (under-5 "over five") false)
(check-expect (under-5 "river") false)
(check-expect (under-5 "") true)

;(define (under-5 str) false)    ;stub
;(define (under-5 str)           ;template
; (... str))

(define (under-5 str)
  (< (string-length str) 5))


;P3

;; Image -> Image
;; Takes inputed image returns the inputed image with a box around it
(check-expect (boxify (ellipse 60 30 "solid" "red")) .) ;Note, image went where dot is

;(define (boxify img)(square 5 "solid" "red"))      ;stub
;(define (boxify img)                               ;template
;  (... img))

(define (boxify img)
  (overlay (rectangle (+ (image-width img)2)
                      (+ (image-height img) 2)
                      "outline"
                      "black")
           img))


;P6

;; Number -> Number
;; doubles n
(check-expect (double 0) 0)
(check-expect (double 4) 8)
(check-expect (double 3.3) (* 2 3.3))
(check-expect (double -1) -2)


#;
(define (double n) 0) ; stub

(define (double n)
  (* 2 n))