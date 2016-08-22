; P1

(require 2htdp/image)

;; alternative-tuition-graph-starter.rkt

;; ===============
;; Constants

(define BAR-WIDTH 60)
(define BAR-COLOR "blue")
(define Y-SCALE 1/200)

(define FONT-COLOR "black")
(define FONT-SIZE 26)

;; ===============
;; Data Definitions

(define-struct school (name tuition next))
;; School is one of:
;;  - false
;;  - (make-school String Natural School)
;; interp. an arbitrary number of schools, where for each school we have its
;;         name and its tuition in USD

(define S1 false)
(define S2 (make-school "Jones" 2000
                        (make-school "Newman" 4000
                                     (make-school "Soloman" 5000 false))))
#;
(define (fn-for-school s)
  (cond [(false? s) (...)]
        [else
         (... (school-name s)
              (school-tuition s)
              (fn-for-school (school-next s)))]))

;; Template rules used
;; - one of: 2 cases
;; - atomic distinct value: false
;; - atomic non-distinct: (school-name s) is String
;; - atomic non-distinct: (school-tuition s) is Natural
;; - self-reference: (school-next s) is School

;; ==============
;; Functions

;; School -> Image
;; creates a bar chart with labeled schools' tution 

(check-expect (chart false) (square 0 "solid" "white"))
(check-expect (chart (make-school "S1" 8000 false))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

(check-expect (chart (make-school "S2" 12000 (make-school "S1" 8000 false))) 
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

;(define (chart S1 ) (square 0 "solid" "white"))   ;stub
;<template from School>

(define (chart s)
  (cond [(false? s) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (overlay/align "center" "bottom"
                                      (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                                      (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                                      (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR))
                       (chart (school-next s)))]))



; P2

;; spinning-bears-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)


;; =================
;; Constants:
(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define ANGULAR-SPEED 3)
(define BEAR-IMG .)   ; image of bear here


;; =================
;; Data definitions:

(define-struct bear (angle x y))
;; Bear is (make-bear Number Number[0, WIDTH] Number[0, HEIGHT])
;; interp. Bear represents the state of the bear
;;  - angle is the angle of the bear in degrees
;;  - x is the x-coordinate
;;  - y is the y-coordinate

(define B1 (make-bear 32 40 59))
(define B2 (make-bear -30 100 200))
#;
(define (fn-for-bear b)
  (... (bear-angle b)  ; Number
       (bear-x b)      ; Number[0, WIDTH]
       (bear-y b)))    ; Number[0, HEIGHT]

;; Template rules used:
;;  - compound: 3 fields

;; ListOfBears is one of
;; - empty
;; - (cons Bear ListOfBears)
;; interp. ListOfBears is a list of bears
(define R1 empty)
(define R2 (cons (make-bear 0 30 40) empty))
(define R3 (cons (make-bear 30 10 53) (cons (make-bear 91 46 20) empty)))

(define (fn-for-lob lob)
  (cond ((empty? lob) (...))
        (else
         (... (fn-for-bear (first lob))
              (fn-for-lob (rest lob))))))


;; Template rules used:
;;  - one of: 2 cases
;;  - atomic definate value: empty
;;  - compound data: 2 fields
;;  - reference: (first lob) is Bear
;;  - self-reference: (rest lob) is ListOfBears

;; =================
;; Functions:

;; ListOfBears -> ListOfBears
;; start the world with (main empty)
;; 
(define (main lob)
  (big-bang lob                       ; ListOfBears
            (on-tick   spin-lob)      ; ListOfBears -> ListOfBears
            (to-draw   render-lob)    ; ListOfBears -> Image
            (on-mouse  create)))      ; ListOfBears Integer Integer MouseEvent -> ListOfBears

;; Bear -> Bear
;; produces correct change to bear-angle based on ANGULAR-SPEED
(check-expect (spin-bear (make-bear 0   41 31)) (make-bear  (+   0 ANGULAR-SPEED) 41 31))
(check-expect (spin-bear (make-bear -50 32 100)) (make-bear (+ -50 ANGULAR-SPEED) 32 100))

;(define (spin-bear B1) (make-bear 0 0 0))  ;stub
;<template from Bear>

(define (spin-bear b)
  (make-bear (+ (bear-angle b) ANGULAR-SPEED) 
             (bear-x b)    
             (bear-y b)))   

;; ListOfBears -> ListOfBears
;; spins bears in entire list used spin-bear
(check-expect (spin-lob empty) empty)
(check-expect (spin-lob (cons (make-bear 0   41 31) empty)) (cons (make-bear  (+   0 ANGULAR-SPEED) 41 31) empty))
(check-expect (spin-lob (cons (make-bear -50 32 100) (cons (make-bear 5 30 40) empty)))
              (cons (make-bear (+ -50 ANGULAR-SPEED) 32 100) (cons (make-bear (+ 5 ANGULAR-SPEED) 30 40) empty)))

;(define (spin-lob lob) empty)  ;stub
;<template from ListOfBears>

(define (spin-lob lob)
  (cond ((empty? lob) empty)
        (else
         (cons (spin-bear (first lob))
               (spin-lob (rest lob))))))

;; Bear Image -> Image
;; renders the rotated image of bear at corresponding (x,y) coordinate on another image
(check-expect (render-bear (make-bear 0 0 0) MTS) (place-image (rotate (modulo 0 360) BEAR-IMG ) 0 0 MTS))
(check-expect (render-bear (make-bear -40 50 20) MTS) (place-image (rotate (modulo -40 360) BEAR-IMG ) 50 20 MTS))

;(define (render-bear b) (square 0 "solid" "white"))  ;stub
;<template from Bear>
(define (render-bear b img)
  (place-image (rotate (modulo (bear-angle b) 360) BEAR-IMG) 
               (bear-x b)  
               (bear-y b)
               img))

;; ListOfBears -> Image
;; renders the list of bears as images
(check-expect (render-lob empty) MTS)
(check-expect (render-lob (cons (make-bear 0 0 0) empty)) (place-image (rotate (modulo 0 360) BEAR-IMG ) 0 0 MTS))
(check-expect (render-lob (cons (make-bear -40 50 20) (cons (make-bear 20 30 50) empty))) (place-image (rotate (modulo -40 360) BEAR-IMG ) 50 20
                                                                                                       (place-image (rotate 20 BEAR-IMG) 30 50 MTS))) 

;(define (render-lob lob) (square 0 "solid" "white"))  ;stub
;<template from ListOfBears>

(define (render-lob lob)
  (cond [(empty? lob) MTS]
        [else 
         (render-bear (first lob) (render-lob (rest lob)))]))

; ListOfBear Integer Integer MouseEvent -> ListOfBear
; creates a new bear at clicked coordinate
(check-expect (create empty 40 50 "button-up") (cons (make-bear 0 40 50) empty))
(check-expect (create empty 0 100 "enter") empty)
(check-expect (create (cons (make-bear 20 40 30) empty) 43 52 "button-up") (cons (make-bear 0 43 52) (cons (make-bear 20 40 30) empty)))

;(define (create lob x y me) empty)   ;stub
;<template from Bear>
(define (create lob x y me)
  (cond ((mouse=? "button-up" me) (cons (make-bear 0 x y) lob))
        (else lob)))


; 

