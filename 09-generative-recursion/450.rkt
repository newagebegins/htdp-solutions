;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |450|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.00001)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Number -> Boolean
(define (poly-root? x)
  (<= (abs (poly x)) ε))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume: f is continuous and monitonically increasing on [left, right]
; assume: (<= (f left) 0 (f right))
; generative: divides interval in half, the root is in one of the two
; halves, picks according to assumption
(check-satisfied (find-root poly 3 6) poly-root?)

(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(<= (f left) 0 f@mid) (find-root f left mid)]
          [(<= f@mid 0 (f right)) (find-root f mid right)]))]))
