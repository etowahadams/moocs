; P1


;; movie-starter.rkt

;; =================
;; Data definitions:

(define-struct movie (name budget released))
;; Movie is (make-movie String Natural Natural)
;; interp. describes attributes of a movie, movie name, budget in dollars, realease year

(define M1 (make-movie "Titanic" 200000000 1997))
(define M2 (make-movie "Avatar"  237000000 2009))
(define M3 (make-movie "The Avengers" 220000000 2012))

#;
(define (fn-for-movie m)
  (... (movie-name     m)    ;String
       (movie-budget   m)    ;Natural
       (movie-released m)))  ;Natural
  
;; Template rules used
;; compound: 3 fields

;; =================
;; Functions:

;; Movie Movie -> String
;; returns the name of the movie which was released more recently
(check-expect (newer-movie M1 M2) "Avatar")
(check-expect (newer-movie M3 M2) "The Avengers")

;(define (newer-movie m1 m2) "")    ;stub

;(define (newer-movie m1 m2)        ;template
;  (... (movie-name m1)
;       (movie-name m2)
;       (movie-budget m1)
;       (movie-budget m2)
;       (movie-released m1)
;       (movie-released m2)))

(define (newer-movie m1 m2)
  (if (> (movie-released m1) (movie-released m2))
      (movie-name m1)
      (movie-name m2)))
       


; P2

;; student-starter.rkt

;; =================
;; Data definitions:

(define-struct student (name grade allergies))
;; Student is (make-student String Natural[1, 12] Boolean)
;; interp. stores information for a student with a name, a grade level (from 1 to 12). true if student has allergies

(define S1 (make-student "Tom"    3 true))
(define S2 (make-student "Sarah" 12 false))

#;
(define (fn-for-student s)
  (... (student-name s)
       (student-grade s)
       (student-allergies s)))

;; Template rules used
;; compound data - 3 fields

;; =================
;; Functions:

;; Student -> Boolean
;; true if student is in grade 6 or below so teacher knows to keep track of allergies
(check-expect (add-name? S1) true)
(check-expect (add-name? S2) false)
(check-expect (add-name? (make-student "Bob" 6  true)) true)
(check-expect (add-name? (make-student "Ned" 11 true)) false)

;(define (add-name? n) false)   ;stub
;<template from Student>

(define (add-name? s)
  (and (<= (student-grade s) 6)
       (student-allergies s)))
              



; P1 HtDW Practice Problem


;; water-balloon-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

;; =================
;; Constants:

(define WIDTH 500)
(define HEIGHT 400)
(define CTR-HEIGHT (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define MOVE-SPEED 3)
(define ROTATE-SPEED 3)
(define WATER-BALLOON.)    ; Balloon image was here


;; =================
;; Data definitions:
(define-struct balloon (x r))
;; Balloon is (make-balloon Number Number)
;; interp. The balloon's changing properties where
;;     - x is the x-coordinate of the ballon and
;;     - r is the angle of the balloon in degrees

(define B1 (make-balloon 3 34))
(define B2 (make-balloon 5 360))

#;
(define (fn-for-balloon n)
  (... (balloon-x n)      ; Number
       (balloon-r n)))    ; Number

;; Template rules used
;; compound data: 2 fields


;; =================
;; Functions:

;; Balloon -> Balloon
;; start the world with (main (make-balloon 0 0))
(define (main b)
  (big-bang b                               ; Balloon
            (on-tick   advance-balloon)     ; Balloon -> Balloon
            (to-draw   render)              ; Balloon -> Image
            (on-key    restart)))           ; Balloon KeyEvent -> Balloon

;; Balloon -> Balloon
;; produce the next state of the balloon x-coordinate and angle based on MOVE-SPEED and ROTATE-SPEED

(check-expect (advance-balloon (make-balloon 2 4)) (make-balloon (+ 2 MOVE-SPEED)(- 4 ROTATE-SPEED)))

;(define (advance-balloon b) B1)   ;stub
;<template from balloon>

(define (advance-balloon b)
  (make-balloon (+ (balloon-x b) MOVE-SPEED)
                (- (balloon-r b) ROTATE-SPEED)))

;; Balloon -> Image
;; renders the image of the balloon at corresponding x-coordinate and angle
(check-expect (render B1) (place-image (rotate (modulo (balloon-r B1) 360) WATER-BALLOON) (balloon-x B1) CTR-HEIGHT MTS))

;(define (render b) (square 0 "solid" "white"))    ;stub
;<template from Balloon>

(define (render b)
  (place-image (rotate (modulo (balloon-r b) 360) WATER-BALLOON) (balloon-x b) CTR-HEIGHT MTS))


;; Balloon  KeyEvent -> Balloon
;; resets the balloon to starting point on spacebar

(check-expect (restart B1 " ") (make-balloon 0 0))

;(define (restart b ke) (make-balloon 0 0))   ;stub
;<template from Balloon>

(define (restart b key)
  (cond ((key=? " " key) (make-balloon 0 0))
        (else b)))

