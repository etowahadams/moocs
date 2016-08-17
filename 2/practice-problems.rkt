
;; P2 


;; demolish-starter.rkt

;; =================
;; Data definitions:

;; BuildingStatus is one of
;; - "old" 
;; - "new"
;; - "heritage"
;; interp. classifications for age of buildings 
;; examples redundant in enumeration
#;
(define (fn-for-BuildingStatus s)
  (cond ((string=? s "old") (...))
        ((string=? s "new") (...))
        ((string=? s "heritage") (...))))

;; Template rules used:
;; one of
;; atomic distinct value: "old"
;; atomic distinct value: "new"
;; atomic distinct value: "heritage"


;; =================
;; Functions:

;; BuildingStatus -> Boolean
;; Produces true if building is classified as "old"
(check-expect (demolish? "old") true)
(check-expect (demolish? "new") false)
(check-expect (demolish? "heritage") false)

;(define (old? s) false)           ;stub
;<template from BuildingStatus>

(define (demolish? s)
  (cond ((string=? s "old") true)
        ((string=? s "new") false)
        ((string=? s "heritage") false)))


;; P3

;; rocket-starter.rkt

;; =================
;; Data definitions:
;; RocketDescent is one of
;; - Number[0, 100]
;; - false
;; interp. the number of kilometers the rocket is to earth, false when done
(define H1 5.2)
(define H6 100)
(define H26 60)
(define H4 false)
#;
(define (fn-for-RocketDescent n)
  (cond ((and (number? n)
              (> n 0)
              (<= n 100))
         (... n))
        (else (...))))

;; Template rules used
;; - atomic non-distinct: Number[0, 100]
;; - atomic distinct value: "Touchdown"


;; =================
;; Functions:
;; RocketDescent -> String
;; outputs the rocket's remaining descent distance in a short string that can be broadcast on Twitter
(check-expect (rocket-descent-to-msg 100) "100km to Earth")
(check-expect (rocket-descent-to-msg 52.1) "521/10km to Earth")         ; 521/10 due to limitation of number-string coersion
(check-expect (rocket-descent-to-msg false) "The rocket has landed!")

;(define (rocket-descent-to-msg n) "")    ;stub
;<template from RocketDescent>

(define (rocket-descent-to-msg n)
  (cond ((and
          (number? n)
          (> n 0)
          (<= n 100))
         (string-append (number->string n) "km to Earth"))
        (else "The rocket has landed!")))

