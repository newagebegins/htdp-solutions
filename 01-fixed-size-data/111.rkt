;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |111|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interp. represents a velocity vector

(define (checked-make-vec x y)
  (if (and (number? x) (>= x 0)
           (number? y) (>= y 0))
      (make-vec x y)
      (error "make-vec: x and y should be positive numbers")))
