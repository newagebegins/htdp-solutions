;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |155|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])

; An RD (short for Russian doll) is one of:
; - String
; - (make-layer String RD)

; RD -> String
; produce the color of the innermost doll
(check-expect (inner "green") "green")
(check-expect (inner (make-layer "red" "blue")) "blue")
(check-expect (inner (make-layer "yellow" (make-layer "blue" "pink"))) "pink")

(define (inner rd)
  (cond
    [(string? rd) rd]
    [(layer? rd) (inner (layer-doll rd))]))
