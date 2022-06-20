;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |039|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define WHEEL-RADIUS 5)

(define (u x)
  (* x WHEEL-RADIUS))

(place-image
 (circle (u 1) "solid" "black")
 (u 8) (u 4)
 (place-image
  (circle (u 1) "solid" "black")
  (u 2) (u 4)
  (place-image
   (rectangle (u 10) (u 2) "solid" "red")
   (u 5) (u 3)
   (place-image
    (rectangle (u 4) (u 2) "solid" "red")
    (u 5) (u 1)
    (empty-scene (u 10) (u 5))))))
