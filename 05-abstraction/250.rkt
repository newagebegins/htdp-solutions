;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |250|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(check-within (tab-sin 2) (list (sin 2) (sin 1) (sin 0)) 0.001)

(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(check-within (tab-sqrt 2) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; [Number -> Number] Number -> [List-of Number]
; tabulates f between n and 0 (incl.) in a list
(check-within (tabulate sin 2) (list (sin 2) (sin 1) (sin 0)) 0.001)
(check-within (tabulate sqrt 2) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

(define (tabulate f n)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate f (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqr between n and 0 (incl.) in a list
(check-within (tab-sqr 2) (list (sqr 2) (sqr 1) (sqr 0)) 0.001)

(define (tab-sqr n)
  (tabulate sqr n))

; Number -> [List-of Number]
; tabulates tan between n and 0 (incl.) in a list
(check-within (tab-tan 2) (list (tan 2) (tan 1) (tan 0)) 0.001)

(define (tab-tan n)
  (tabulate tan n))
