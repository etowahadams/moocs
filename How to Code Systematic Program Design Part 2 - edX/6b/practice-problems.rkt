;; P3

;; hp-family-tree-starter.rkt

;; Data Definitions

(define-struct person (name decendants patronus wand))
;; Person is (make-person String ListOfPerson String String)
;; interp. person represents a charater
;;   name       - the name of person
;;   decendants - the kids of the character
;;   patronus   - the name of the patronus the person has, empty string means unknown
;;   wand       - the type of wand, empty string means unknown

(define P1 (make-person "Harry" empty "Stag" "holly"))
(define AUTHUR (make-person "Authur" (cons (make-person "Lily" empty "" "Oak")
                                           (cons (make-person "Victoire" empty "" "")
                                                 (cons (make-person "Albus" empty "" "")
                                                       (cons (make-person "James" empty "" "Oak") empty))))
                            "Weasel"
                            ""))

;; ListOfPerson is one of
;; - empty
;; - (cons Person ListOfPerson)
;; interp. ListOfPerson is a list of decendants of a person

(define L1 (cons (make-person "Lily" empty "" "")
                 (cons (make-person "Victoire" empty "" "")
                       (cons (make-person "Albus" empty "" "")
                             (cons (make-person "James" empty "" "") empty)))))

(define (fn-for-person n)
  (... (person-name n)
       ((fn-for-lop (person-decendants n))
        (person-patronus n)
        (person-wand n))))

(define (fn-for-lod n)
  (cond [(empty? n) (...)]
        [else
         (... (fn-for-person (first n))
              (fn-for-lod (rest n)))]))

;; ListOfPair is one of:
;; - empty
;; - (cons (list String String) ListOfPair)
;; interp. a pair list (i.e. list of two-element lists)

(define LOP1 (list (list "Harry" "Stag")))

(define (fn-for-lopa n)
  (cond [(empty? n) (...)]
        [else
         (... (first (first n))
              (second (first n))
              (fn-for-lopa (rest n)))]))

;; ListOfString is one of:
;; - empty
;; - (cons string ListOfString)
;; interp. a list of strings

(define L7 empty)
(define L2 (list "bal" "red" "rem"))



;; ============
;; Functions


;; Person -> ListOfPair
;; ListOfPerson -> ListOfPair
;; produces a pair list (i.e. list of two-element lists) of every person in the tree and his or her patronus.

(check-expect (patronus--p P1) (list (list "Harry" "Stag")))
(check-expect (patronus--p AUTHUR) (list (list "Authur" "Weasel") (list "Lily" "") (list "Victoire" "") (list "Albus" "") (list "James" "")))
(check-expect (patronus--lop empty) empty)

;(define (patronus-list n) empty)   ;stub
;<template from Person>

(define (patronus--p n)
  (cons (list (person-name n) (person-patronus n))
        (patronus--lop (person-decendants n))))

(define (patronus--lop n)
  (cond [(empty? n) empty]
        [else
         (append (patronus--p (first n))
                 (patronus--lop (rest n)))]))


;; Person String -> ListOfString
;; ListOfPerson String -> ListOfString
;; produces the names of all descendants of a given person whose wands are made of a given material

(check-expect (wand-type--lop empty "Holly") empty)
(check-expect (wand-type--p AUTHUR "Oak") (list "Lily" "James"))
(check-expect (wand-type--p (make-person "a" empty "b" "c") "c") (list "a"))

;<template from Person and ListOfPerson with additional atomic parameter>

(define (wand-type--p n type)
  (if (string=? (person-wand n) type)
                (cons (person-name n) (wand-type--lop (person-decendants n) type))
                (wand-type--lop (person-decendants n) type)))

(define (wand-type--lop n type)
  (cond [(empty? n) empty]
        [else
         (append (wand-type--p (first n) type)
                 (wand-type--lop (rest n) type))]))