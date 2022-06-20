;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |104|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vehicle [passengers plate fuel])
; A Vehicle is a structure:
;   (make-vehicle Natural String Number)
; interp. (make-vehicle p pl f) is a vehicle that can carry p passengers,
;         has license plate pl and its fuels consumption is f miles per gallon.

(define (fn-for-vehicle v)
  (... (vehicle-passengers v) ... (vehicle-plate v) ... (vehicle-fuel v) ...))
