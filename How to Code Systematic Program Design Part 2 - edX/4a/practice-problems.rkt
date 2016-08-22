; P2


;; double-starter.rkt

;; =================
;; Data definitions:

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

;; ListOfNumbers -> ListOfNumber
;;  consumes a list of numbers and doubles every number in the list
(check-expect (double-all empty) empty)
(check-expect (double-all LON2) (cons 120 (cons 84 empty)))
(check-expect (double-all (cons 20 (cons 90 (cons 32 empty)))) (cons 40 (cons 180 (cons 64 empty))))

;(define (double-all lon) 0)  ;stub
;<template from ListOfNumber>

(define (double-all lon)
  (cond [(empty? lon) empty]
        [else
         (cons (* 2 (first lon))
              (double-all (rest lon)))]))



; P3

;; boolean-list-starter.rkt

;; =================
;; Data definitions:

;; ListOfBoolean is one of
;; - empty
;; - (cons Boolean ListOfBoolean)
;; interp. a list of booleans
(define B1 empty)
(define B2 (cons true (cons false empty)))

#;
(define (fn-for-lob lob)
  (cond ((empty? lob) (...))
        (else 
         (... (first lob)
              (fn-for-lob (rest lob))))))


;; Template rules used
;; - one of: 2 cases
;; - atomic distinct value: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (rest lob) is ListOfBoolean

;; =================
;; Functions:

;; ListOfBoolean -> Boolean
;; returns true if all values in ListOfBoolean are true
(check-expect (all-true? empty) true)
(check-expect (all-true? (cons true (cons false empty))) false)
(check-expect (all-true? (cons true empty)) true)
(check-expect (all-true? (cons true (cons true empty))) true)

;(define (all-true? lob) false)   ;stub
; <template from ListOfBoolean>


(define (all-true? lob)
  (cond ((empty? lob) true)
        (else 
         (if  (first lob)
              (all-true? (rest lob))
               false))))




; P5


;; largest-starter.rkt

;; =================
;; Data definitions:

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

;; ListOfNumber -> Number
;; produces the largest number in the list, 0 if the list is empty
(check-expect (largest empty) 0)
(check-expect (largest (cons 30 empty)) 30)
(check-expect (largest (cons 20 (cons 30 (cons 40 empty)))) 40)
(check-expect (largest (cons 15 (cons 15 empty))) 15)

;(define (largest lon) 0)   ;stub
;<template from ListOfNumber>

(define (largest lon)
  (cond [(empty? lon) 0]
        [else
         (if (>= (first lon) (largest (rest lon)))
             (first lon)
             (largest (rest lon)))]))


; P6

(require 2htdp/image)

;; image-list-starter.rkt

;; =================
;; Data definitions:

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. ListOfImage is a list of images
(define I1 empty)
(define I2 (cons (square 12 "solid" "red") empty))
(define I3 (cons (circle 2 "solid" "blue") (cons (square 20 "solid" "red") empty)))

#;
(define (fn-for-loi loi)
  (cond ((empty? loi) (...))
        else
        (... (first loi)
             (fn-for-loi (rest loi)))))

;; Template rules used:
;; - one of: 2 cases
;; - atomic definate value: empty
;; - compound: (cons Image ListOfImage)
;; - self-reference: (rest loi) is ListOfImages

;; =================
;; Functions:

;; ListOfImages -> Number
;; consumes a list of images and produces a number that is the
;; sum of the areas of each image area is defined as width x height

(check-expect (total-area empty) 0)
(check-expect (total-area (cons (square 5 "solid" "blue") (cons (rectangle 2 3 "solid" "yellow") empty))) 31)

;(define (total-area loi) 0)    ;stub
;<template from ListOfImages>

(define (total-area loi)
  (cond ((empty? loi) 0)
        (else
        (+ (*(image-height (first loi))
             (image-width  (first loi)))
           (total-area (rest loi))))))
