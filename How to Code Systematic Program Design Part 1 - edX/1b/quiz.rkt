(require 2htdp/image)

;Image Image -> Boolean
;Returns true if area of the first rectangle is larger than the second rectangle.
(check-expect (bigger (rectangle 10 20 "solid" "red") (rectangle 10 30 "solid" "red")) false)
(check-expect (bigger (rectangle 4 5 "solid" "black") (rectangle 2 3 "solid" "blue")) true)
(check-expect (bigger (rectangle 3 4 "solid" "orange") (rectangle 4 3 "solid" "green")) false)


;(define (bigger img1 img2) false)   ;stub
;(define (bigger img1 img2)          ;template
;    (... img1 img2)

(define (bigger img1 img2)
  (> (* (image-width img1)
        (image-height img1))
     (* (image-width img2)
        (image-height img2))))

