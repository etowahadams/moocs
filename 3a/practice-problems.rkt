(require 2htdp/image)
(require 2htdp/universe)

; P1


;; countdown-animation starter.rkt

;; =================
;; Constants:

(define WIDTH 300)
(define HEIGHT 300)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 100)
(define TEXT-COLOR "black")

;; =================
;; Data definitions:

;; Countdown is Natural[0, 10]
;; interp. number of seconds left in countdown
(define C1 10)    ; just started
(define C5  3)
(define C10 0)    ; finished

#;
(define (fn-for-countdown c)
  (... c))

;; Template rules used
;; atomic non-distinct value: Natural[0, 10]

;; =================
;; Functions:

;; Countdown -> Countdown
;; start the world with (main 10)
;; 
(define (main c)
  (big-bang c                                 ; Countdown
            (on-tick   advance-countdown 1)   ; Countdown -> Countdown
            (to-draw   render)                ; Countdown -> Image
            (on-key handle-key)))             ; Countdown KeyEvent -> Countdown

;; Countdown -> Countdown
;; produce the next number in the countdown
(check-expect (advance-countdown 10) 9)
(check-expect (advance-countdown 5) (- 5 1))
(check-expect (advance-countdown 0) 0)

;(define (advance-countdown c) 0)   ;stub
;<used template from Countdown>

(define (advance-countdown c)
  (if (< 0 c)
      (- c 1)
      0))
      
;; Countdown -> Image
;; renders image from countdown
(check-expect (render 10) (place-image (text "10" TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))
(check-expect (render  0) (place-image (text  "0" TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))

;(define (render c) MTS)     ;stub
;<used template from Countdown>

(define (render c)
  (place-image (text (number->string c) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))

;; Countdown KeyEvent -> Countdown
;; Resets counter to 10 with space bar
(check-expect (handle-key 0  " ") 10);
(check-expect (handle-key 10 " ") 10);
(check-expect (handle-key 5  "a")  5);

;(define (handle-key c ke) 10)   ;stub
;<used template from Countdown>

(define (handle-key c ke)
  (cond [(key=? ke " ") 10]
        [else c]))




; P2

;; traffic-light-starter.rkt

;; =================
;; Constants:

(define WIDTH    100)
(define HEIGHT   200)
(define CTR-X    (/ WIDTH  2))
(define CTR-Y    (/ HEIGHT 2))
(define MTS      (empty-scene WIDTH HEIGHT))
(define R-LIGHT .)                          ; Trafic light image here
(define G-LIGHT .)                          ; Trafic light image here
(define Y-LIGHT .)                          ; Trafic light image here

;; =================
;; Data definitions:

;; Light one of:
;; - "red"
;; - "green"
;; - "yellow"
;; interp. The traffic light can be red, yellow, or geen
;; examples redundant for enumeration

#;
(define (fn-for-light l)
  (cond [(string=? l "red")     (...)]
        [(string=? l "green")   (...)]
        [(string=? l "yellow")  (...)]))
        

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct value: "red"
;; - atomic distinct value: "green"
;; - atomic distinct value: "yellow"


;; =================
;; Functions:

;; Light -> Light
;; start the world with (main "red")
;; 
(define (main l)
  (big-bang l                        ; Light
            (on-tick   change 1)     ; Light -> Light
            (to-draw   render)))     ; Light -> Image

;; Light -> Light
;; produce the next color light in the sequence
(check-expect (change "red")    "green")
(check-expect (change "green")  "yellow")
(check-expect (change "yellow") "red")

;(define (change l) "red")   ;stub
; <used template from Light>
(define (change l)
  (cond [(string=? l "red")    "green"]
        [(string=? l "green")  "yellow"]
        [(string=? l "yellow") "red"]))


;; LIGHT -> Image
;; Produces corresponding image for light
(check-expect (render "red")    (place-image R-LIGHT CTR-X CTR-Y MTS))
(check-expect (render "green")  (place-image G-LIGHT CTR-X CTR-Y MTS))
(check-expect (render "yellow") (place-image Y-LIGHT CTR-X CTR-Y MTS))

;(define (render l) G-LIGHT)   ;stub
;<used template from Light>

(define (render l)
  (cond [(string=? l "red")    (place-image R-LIGHT CTR-X CTR-Y MTS)]
        [(string=? l "green")  (place-image G-LIGHT CTR-X CTR-Y MTS)]
        [(string=? l "yellow") (place-image Y-LIGHT CTR-X CTR-Y MTS)]))
 

