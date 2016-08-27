(require 2htdp/image)
;; SPD2-Design-Quiz-1.rkt


;; ======================================================================
;; Constants
(define COOKIES .)

;; ======================================================================
;; Data Definitions

;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful                   
              (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: 2 fields
;;  - self-reference: (sub1 n) is Natural

; P1

;; Natural Image -> Image
;; produce an n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

;(define (pyramid n i) empty-image) ; stub

(define (pyramid n i)
  (cond [(zero? n) empty-image]
        [else
         (above (pyramid (sub1 n) i)
                (row n i))]))

;; Natural Image -> Image
;; produces row of n images
(check-expect (row 0 COOKIES) empty-image)
(check-expect (row 3 COOKIES) (beside COOKIES COOKIES COOKIES))

; (define (row n i) empty-image)

(define (row n i)
  (cond [(zero? n) empty-image]
        [else
         (beside i (row (sub1 n) i ))]))

; P2

;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one

(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))
;(define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (first-sink (first lob) (sink (rest lob)))]))

;; Blob ListOfBlob -> ListOfBlob
;; orders the blob, moves solids down one position
(check-expect (first-sink "solid" (cons "solid" (cons "bubble" empty)))
              (cons "solid" (cons "solid" (cons "bubble" empty))))
(check-expect (first-sink "bubble" (cons "bubble" (cons "solid" empty)))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))

(define (first-sink b lob)
  (cond [(empty? lob) (cons b lob)]
        [else
         (if (solid-blob? b)
             (cons (first lob) (cons b (rest lob)))
             (cons b lob))]))

;; Blob -> Boolean
; true if blob is solid
(check-expect (solid-blob? "solid") true)
(check-expect (solid-blob? "bubble") false)

; (define (solid-blob? b) false)    ;stub

(define (solid-blob? b)
  (string=? b "solid"))