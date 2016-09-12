;; P2

;; ListOfNumber ListOfNumber -> ListOfNumber
;; procudes sorted list of numbers by merging a sorted lists

(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1)) (list 1))
(check-expect (merge (list 2) empty) (list 2))
(check-expect (merge (list 1) (list 2)) (list 1 2))
(check-expect (merge (list 2 3) (list 1 4)) (list 1 2 3 4))

;(define (merge lon1 lon2) empty)   ;stub

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (if (<= (first lon1) (first lon2))
             (cons (first lon1)
                   (merge (rest lon1) lon2))
             (cons (first lon2)
                   (merge (rest lon2) lon1)))]))




;; P4

;; ==========
;; Functions:

;; Pattern ListOf1String -> Boolean
;; produces true if the pattern matches the ListOf1String
(check-expect (match? empty empty) true)
(check-expect (match? empty (list "A")) true)
(check-expect (match? (list "A") empty) false)
(check-expect (match? (list "A") (list "L")) true)
(check-expect (match? (list "A" "N" "A") (list "P" "3" "O")) true)
(check-expect (match? (list "A" "N" "N") (list "P" "3" "O")) false)
(check-expect (match? (list "A" "N" "N") (list "P" "3" "6" "Z" "R")) true)

;(define (match? p los) false)  ;stub


(define (match? pat los)
  (cond [(empty? pat) true]                            
        [(empty? los) false]                          
        [(string=? (first pat) "A")                    
         (and (alphabetic? (first los))
              (match? (rest pat) (rest los)))]
        [(string=? (first pat) "N")                    
         (and (numeric? (first los))
              (match? (rest pat) (rest los)))]))



;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))