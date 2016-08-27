; P2

(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position

(check-expect (handle-mouse empty 30 40 "button-down")
              (cons (make-drop 30 40) empty))
(check-expect (handle-mouse (cons (make-drop 22 49) (cons (make-drop 42 30) empty)) 91 31 "button-down")
              (cons (make-drop 91 31)
                    (cons (make-drop 22 49)
                          (cons (make-drop 42 30) empty))))
(check-expect (handle-mouse empty 20 40 "enter") empty)
(check-expect (handle-mouse (cons (make-drop 81 21) empty) 11 41 "leave") (cons (make-drop 81 21) empty))

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y me)
  (if (mouse=? "button-down" me) 
      (cons (make-drop x y) lod)
      lod))

;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
;; !!!
(check-expect (next-drops empty) empty)
(check-expect (next-drops (cons (make-drop 20 (+ HEIGHT 10)) (cons (make-drop 40 10) empty)))
              (cons (make-drop 40 (+ 10 SPEED)) empty))
(check-expect (next-drops (cons (make-drop 20 (+ HEIGHT 10)) (cons (make-drop 30 (+ HEIGHT 20)) empty))) empty)
(check-expect (next-drops (cons (make-drop 42 HEIGHT) empty)) empty)

;(define (next-drops lod) empty) ; stub
(define (next-drops lod)
  (filter-drops (tick-drops lod)))

;; ListOfDrop -> ListOfDrop
;; creates a list of advanced drops using add-speed
(check-expect (tick-drops empty) empty)
(check-expect (tick-drops (cons (make-drop 20 32) (cons (make-drop 20 30) empty)))
              (cons (make-drop 20 (+ SPEED 32)) (cons (make-drop 20 (+ SPEED 30)) empty)))

;(define (tick-drops lod) lod)  ;stub
;<template from ListOfDrop>

(define (tick-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (add-speed (first lod))
               (tick-drops (rest lod)))]))

;; Drop -> Drop
;; increases y of drop by SPEED
(check-expect (add-speed (make-drop 30 2)) (make-drop 30 (+ SPEED 2)))
;(define (add-speed d) (make-drop 0 0))  ;stub
; <template from Drop>

(define (add-speed d)
  (make-drop (drop-x d) 
             (+ SPEED (drop-y d))))

;; ListOfDrop -> ListOfDrop
;; filters out drops greater than HEIGHT
(check-expect (filter-drops empty) empty)
(check-expect (filter-drops (cons (make-drop 20 HEIGHT) (cons (make-drop 15 10) empty)))
              (cons (make-drop 15 10) empty))
(check-expect (filter-drops (cons (make-drop 90 (+ 20 HEIGHT)) (cons (make-drop 30 (+ 2 HEIGHT)) empty))) empty)

;(define (filter-drops lod) empty)   ;stub
;<template from ListOfDrop>

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if  (< (drop-y (first lod)) HEIGHT)
              (cons (first lod) (filter-drops (rest lod)))
              (filter-drops (rest lod)))]))

;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 20 30) empty)) (place-image DROP 20 30 MTS))
(check-expect (render-drops (cons (make-drop 10 20) (cons (make-drop 20 10) empty)))
                            (place-image DROP 10 20
                                         (place-image DROP 20 10 MTS)))
;(define (render-drops lod) MTS) ; stub
;<template from ListOfDrop>

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image DROP (drop-x (first lod)) (drop-y (first lod))
                      (render-drops (rest lod)))]))
