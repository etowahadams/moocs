(require 2htdp/image)

;  PROBLEM 1:


(define CUT-OFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (circle-fractal CUT-OFF "red") (circle 5 "outline" "red"))
(check-expect (circle-fractal (* 2 CUT-OFF) "blue") (overlay (circle (* 2 CUT-OFF) "outline" "blue")
                                                             (beside (circle CUT-OFF "outline" "blue") (circle CUT-OFF "outline" "blue"))))                                                                     
;(define (circle-fractal n c) empty-image) ;stub
(define (circle-fractal n c)
  (if (<= n CUT-OFF)
      (circle n "outline" c)
      (local [(define outer (circle n "outline" c))
              (define inner (circle-fractal (/ n 2) c))]
        (overlay outer
                 (beside inner inner)))))






;  PROBLEM 2:

;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))
(define B4 (list "X"  "X"  "O"     ; a complete board
                 "O"  "X"  "O"
                 "X"  "O" "X"))

(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))

;; Board -> (listof Board) or false
;; produces all possible filled boards that are reachable from the current board, false if not
(check-expect (solvet B4) (list B4))
(check-expect (solvet B2) (list (list "X"  "X"  "O"   
                                      "O"  "X"  "O"
                                      "X"  "X"  "X")
                                (list "X"  "X"  "O"    
                                      "O"  "X"  "O"
                                      "X"  "O" "X")))
(check-expect (solvet B3) (list (list "X" "O" "X"      
                                      "O" "O" "X"
                                      "X" "X" "X")
                                (list "X" "O" "X"       
                                      "O" "O" "X"
                                      "X" "X" "O")
                                (list "X" "O" "X"       
                                      "O" "O" "O"
                                      "X" "X" "X")
                                (list "X" "O" "X"       
                                      "O" "O" "O"
                                      "X" "X" "O")))


(define (solvet bd)
  (local [(define (solvet-bd bd)
            (if (full? bd)
                (list bd)
                (solvet-lob (next-bd bd))))
          (define (solvet-lob lob)
            (cond [(empty? lob) empty]
                  [else
                   (append (solvet-bd (first lob))
                           (solvet-lob (rest lob)))]))]
    (solvet-bd bd)))

;; Board -> Boolean
;; checks if board is full or not
(check-expect (full? B4) true)
(check-expect (full? B2) false)

(define (full? bd)
  (andmap string? bd))

;; Board -> (listof Board)
;; generates the next two possibilities of the board
(check-expect (next-bd B2) (list (list "X"  "X"  "O"   
                                       "O"  "X"  "O"
                                       "X"  "X"  "X")
                                 (list "X"  "X"  "O"     
                                       "O"  "X"  "O"
                                       "X"  "O" "X")))
(check-expect (next-bd B3) (list (list "X" "O" "X"     
                                       "O" "O" "X"
                                       "X" "X" false)
                                 (list "X" "O" "X"       
                                       "O" "O" "O"
                                       "X" "X" false)))
(define (next-bd bd)
  (replace-XO bd (false-pos bd)))

;; Board -> Pos
;; finds the position of the first blank space of the board
(check-expect (false-pos B2) 7)
(check-expect (false-pos B0) 0)

(define (false-pos bd)
  (cond [(empty? bd) error]
        [else
         (if (false? (first bd))
             0
             (+ 1 (false-pos (rest bd))))]))

;; Pos Board -> (listof Board)
;; creates a list which has inserted an "X" and "O" at given position p on board
(check-expect (replace-XO B2 7) (list (list "X"  "X"  "O"    
                                            "O"  "X"  "O"
                                            "X"  "X"  "X")
                                      (list "X"  "X"  "O"     
                                            "O"  "X"  "O"
                                            "X"  "O" "X")))
(define (replace-XO bd p)
  (list (insert bd p "X") (insert bd p "O")))

;; Board Pos Val -> Board
;; inserts Val into the position on board

(check-expect (insert B2 7 "X") (list "X"  "X"  "O"    
                                      "O"  "X"  "O"
                                      "X"  "X"  "X"))
(check-expect (insert (list false "X") 0 "X") (list "X" "X"))

(define (insert bd p nv)  
  (cond [(zero? p) (cons nv (rest bd))]
        [else
         (cons (first bd)
               (insert (rest bd) (sub1 p) nv))]))

;  PROBLEM 3:


;; Board -> (listof Board) or false
;; produces boards that would be produced with alternating X's and O's, with X staring first
(check-expect (correct B4) (list B4))
(check-expect (correct B2) (list  (list "X"  "X"  "O"    
                                        "O"  "X"  "O"
                                        "X"  "O" "X")))

(check-expect (correct B3) (list (list "X" "O" "X"       
                                       "O" "O" "X"
                                       "X" "X" "O")
                                 (list "X" "O" "X"       
                                       "O" "O" "O"
                                       "X" "X" "X")))


(define (correct bd)
  (local [(define (5X4O bd)          ; Board -> Boolean
            (and (= 5 (amount "X" bd)) (= 4 (amount "O" bd))))]
    (filter 5X4O (solvet bd))))

;; String Board -> Number
;; counts how many of string is in the board
(check-expect (amount "X" (list "X" "X")) 2)
(check-expect (amount "O" (list "X" "O")) 1)

(define (amount val bd)  
  (cond [(empty? bd) 0]
        [else
         (if (string=? val (first bd))
             (+ 1 (amount val (rest bd)))
             (+ 0 (amount val (rest bd))))]))