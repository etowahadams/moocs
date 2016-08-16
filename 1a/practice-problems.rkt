(require 2htdp/image)

;P1

(* 3 5 7)
(* (* 3 5) 7)


;P3

(above (beside (square 10 "solid" "blue")
               (square 10 "solid" "yellow"))
       (beside (square 10 "solid" "yellow")
               (square 10 "solid" "blue")))


;P5

(define IMAGE1 (rectangle 10 15 "solid" "red"))
(define IMAGE2 (rectangle 15 10 "solid" "red"))

(> (image-height IMAGE1) (image-height IMAGE2))
(< (image-width IMAGE1) (image-width IMAGE2))

(and (= (image-height IMAGE1) (image-height IMAGE2))
     (= (image-width IMAGE1) (image-width IMAGE2)))


;P15

(define (larger x y)
  (if (> x y)
      x
      y))