;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |081|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct time [hours minutes seconds])
; A Time is a structure:
;   (make-time Natural[0-23] Natural[0-59] Natural[0-59])
; interp. A point in time since midnight

; Time -> Natural
; produce the number of seconds that have passed since midnight
(check-expect (time->seconds (make-time 0 0 23)) 23)
(check-expect (time->seconds (make-time 0 1 5)) 65)
(check-expect (time->seconds (make-time 2 3 5)) (+ (* 2 60 60) (* 3 60) 5))
(check-expect (time->seconds (make-time 12 30 2)) 45002)
(define (time->seconds t)
  (+ (* (time-hours t) 60 60) (* (time-minutes t) 60) (time-seconds t)))
