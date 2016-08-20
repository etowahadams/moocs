;; HtDD Design Quiz

;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural


; Problem 1:

;; Age -> Boolean
;; produces true if age is a teenager (i.e., between the ages of 13 and 19)
(check-expect (teenager? 13) true)
(check-expect (teenager? 19) true)
(check-expect (teenager? 16) true)
(check-expect (teenager? 42) false)
(check-expect (teenager? 11) false)

;(define (teenager? n) false)    ;stub
; <template from Age>

(define (teenager? a)
  (< 12 a 20))


; Problem 2:

;; MonthAge is Natural
;; intrep. a person's age in months
(define p1 32)
(define p2 12)

#;
(define (fn-for-MonthAge n)
  (... n))

;; Template rules used:
;; - atomic non-distinct value: Natural



; Problem 3:

;; Age -> MonthAge
;; takes person's age and returns the equivalent in months
(check-expect (months-old 1) 12)
(check-expect (months-old 57) (* 12 57))

;(define (months-old n) 12)          ;stub
;<template from MonthAge>

(define (months-old n)
  (* 12 n))


; Problem 4:

;; Health is one of
;; - Natural    - of lives
;; - False
;; interp. represents health of a charater, false when charater is dead
(define char1 3)
(define char2 0)
(define char4 false)

#;
(define (fn-for-Health c)
  (cond ((and (number? c)
              (>= c 0))
         (... c))
        (else (...))))


;; Template rules used
;; - one of: 2 cases
;; - atomic non-definate value: Natural
;; - atomic definate value: false

;; Health -> Health
;; increases the lives of a character has 0 or more lives, otherwise is still dead,
;; which is represented as "false"
(check-expect (increase-health 3) 4)
(check-expect (increase-health 0) 1)
(check-expect (increase-health false) false)

;(define (increase-health h) false)      ;stub
;<template from Health

(define (increase-health c)
  (cond ((and
          (number? c)
          (>= c 0))
         (+ c 1))
        (else false)))
