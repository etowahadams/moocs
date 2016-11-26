(require 2htdp/image)
(require 2htdp/universe)

; P1

;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)

;; Number -> Image
;; draws a circle fractal starting with size n)

;; check-expects from solution
(check-expect (circle-fractal (/ TRIVIAL-SIZE STEP))
              (local [(define center (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))
                      (define leaf (circle TRIVIAL-SIZE "solid" "blue"))]
                (above leaf
                       (beside leaf center leaf)
                       leaf)))
(check-expect (circle-fractal (/ TRIVIAL-SIZE (sqr STEP)))
              (local [(define center (circle (/ TRIVIAL-SIZE (sqr STEP)) "solid" "blue"))
                      (define leaf-center (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))
                      (define leaf-leaf (circle TRIVIAL-SIZE "solid" "blue"))
                      (define top-leaf (above leaf-leaf
                                              (beside leaf-leaf leaf-center leaf-leaf)))]
                (above top-leaf
                       (beside (rotate 90 top-leaf) center (rotate -90 top-leaf))
                       (rotate 180 top-leaf))))

(define (circlef n)
  (local [(define center (circle n "solid" "blue"))
          (define around (draw (* n STEP)))]
    (above around
           (beside (rotate 90 around) center (rotate -90 around))
           (rotate 180 around))))

;; Number -> Image
;; draws all but one of the surrounding circle fracs
(define (draw n)
  (if (<= n TRIVIAL-SIZE)
      (circle n "solid" "blue")
      (local [(define center (circle n "solid" "blue"))
              (define around (draw (* n STEP)))]
        (above around
               (beside (rotate 90 around) center (rotate -90 around))))))

; P2


;; Constants
;; ===============
(define WIDTH 500)
(define HEIGHT 500)

(define CUTOFF 5)
(define BAR-HEIGHT 10)
(define SPACE-HEIGHT 5)



;; =================
;; Data definitions:

;; WorldState is a number
;; interp. the x position of mouse 
(define WS1 31)
(define WS2 143)

#;
(define (fn-for-ws ws)
  (... ws))


;; =================
;; Functions:

;; WorldState -> WorldState
;; start the world with (main 0)
;; 
(define (main ws)
  (big-bang ws
            (on-draw render)
            (on-mouse mouse-handle)))      ; WS Integer Integer MouseEvent -> WS

;; WS -> Image
;; render a cantor with percent width ws
(check-expect (render 150) (cantor WIDTH (/ 150 WIDTH)))
(check-expect (render 243) (cantor WIDTH (/ 243 WIDTH)))

(define (render ws)
  (cantor WIDTH (/ ws WIDTH)))

;; WorldState Integer Integer MouseEvent -> WorldState
;; changes the worldstate according to x position of the mouse

(check-expect (mouse-handle 10 20 20 "move") 20)
(check-expect (mouse-handle 10 10 20 "move") 10)
(check-expect (mouse-handle 30 40 10 "button-up") 30)

(define (mouse-handle ws x y mevt)
  (cond [(mouse=? mevt "move") x]
        [else ws]))

;; Number Number-> Image
;; produces produces a cantor with width w and has r precent wide spacing

(check-expect (cantor CUTOFF (/ 1 3)) (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))
(check-expect (cantor (* CUTOFF 3) (/ 1 3)) (above (rectangle (* CUTOFF 3) 10 "solid" "blue")
                                           (rectangle (* CUTOFF 3) SPACE-HEIGHT "solid" "white")
                                           (beside (rectangle CUTOFF BAR-HEIGHT "solid" "blue")
                                                   (rectangle CUTOFF BAR-HEIGHT "solid" "white")
                                                   (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))))
;(define (cantor CUTOFF) empty-image)  ;stub

(define (cantor w r)
  (if (<= w CUTOFF)
      (rectangle w BAR-HEIGHT "solid" "blue")      
      (local [(define wc  (* w r))            ;white block width
              (define ws  (/ (- w wc) 2))     ;blue block width
              (define ctr (rectangle wc BAR-HEIGHT "solid" "white"))
              (define next (cantor ws r))]
        (above (rectangle w BAR-HEIGHT "solid" "blue")
               (rectangle w SPACE-HEIGHT   "solid" "white")
               (beside next ctr next))))) 