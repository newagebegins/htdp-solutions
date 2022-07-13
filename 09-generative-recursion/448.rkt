;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |448|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.001)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Number -> Boolean
(define (poly-root? x)
  (<= (abs (poly x)) ε))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume: f is continuous
; assume: (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative: divides interval in half, the root is in one of the two halves, picks according to assumption
; termination: on each recursion an interval is halved, so after some number of steps we get
; the interval that is smaller than ε. If initial interval is S1, then we need n >= (log(S1) - (log(ε))/log2 steps to find a root
(check-satisfied (find-root poly 3 6) poly-root?)
(check-satisfied (find-root poly 1 6) poly-root?)

(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))
