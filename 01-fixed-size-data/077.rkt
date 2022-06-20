;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |077|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct point-in-time [hours minutes seconds])
; A PointInTime is a structure:
;   (make-point-in-time Integer[0-23] Integer[0-59] Integer[0-59])
; interp. A point in time since midnight
