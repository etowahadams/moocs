
; Problem 1:

;; Player is String
;; interp.  the name of a tennis player.
(define P0 "Maria")
(define P2 "Serena")

#;
(define (fn-for-player p)
  (... p))



;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; interp.  a team roster, ordered from best player to worst.
(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))



(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp.  a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))



;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))


;; Roster Roster -> Boolean
;; true if all players in both rosters can play, false if not
(check-expect (all-play? empty empty) true)
(check-expect (all-play? empty (cons "Gene" empty)) false)
(check-expect (all-play? (cons "Ern" empty) empty) false)
(check-expect (all-play? (list "Bob" "Shelly" "Pop") (list "Ron" "Ben" "Eve")) true)
(check-expect (all-play? (list "Bob" "Shelly" "Pop") (list "Ron" "Ben")) false)

;(define (all-play? r1 r2) false)  ;stub
;<template from Roster>

(define (all-play? r1 r2)
  (cond [(and (empty? r1) (empty? r2)) true]
        [(or  (empty? r1) (empty? r2)) false]
        [else (all-play? (rest r1) (rest r2))]))

;                      |  empty        |    (cons Person Roster)
; _____________________|_______________|_________________________              
; empty                | true          |    false
; _____________________|_______________|________________________
; (cons Person Roster) | false         |   (all-play <rest>)



; Problem 2:


;; Roster Roster -> ListOfMatch
;; given two teams, produces the list of tennis matches that will be played
(check-expect (match-maker empty empty) empty)
(check-expect (match-maker (list "Mike" "Ben" "Gene") (list "Sara" "Jan" "Jack"))
              (list (make-match "Mike" "Sara") (make-match "Ben" "Jan") (make-match "Gene" "Jack")))


;(define (match-maker r1 r2) empty)  ;stub

(define (match-maker r1 r2)
  (cond [(empty? r1) empty]
        [else 
         (cons (make-match (first r1) (first r2))
              (match-maker (rest r1) (rest r2)))]))

;                      |  empty        |    (cons Person Roster)
; _____________________|_______________|________________________________            
; empty                | empty         |    N/A
; _____________________|_______________|________________________________
; (cons Person Roster) | N/A           |   (cons (make-match <first>)
;                      |               |   (cons (make-match <rest>)))
; _____________________|_______________|________________________________
;                                                      
