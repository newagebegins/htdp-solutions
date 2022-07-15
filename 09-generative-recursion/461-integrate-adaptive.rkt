;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 461-integrate-adaptive) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(define (integrate-kepler f a b)
  (* 0.5 (- b a) (+ (f a) (f b))))

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)

(define (integrate-adaptive f a b)
  (local ((define mid (/ (+ a b) 2))
          (define t1 (integrate-kepler f a b))
          (define t2 (+ (integrate-kepler f a mid)
                        (integrate-kepler f mid b))))
    (cond
      [(< (abs (- t1 t2)) (* ε (- b a))) t2]
      [else (+ (integrate-adaptive f a mid)
               (integrate-adaptive f mid b))])))
