;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |094|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define BGW 200)
(define BGH 400)
(define UFO (overlay (ellipse 30 10 "solid" "blue") (circle 8 "solid" "blue")))
(define TANK (rectangle 20 10 "solid" "orange"))
(define BG (empty-scene BGW BGH))
(define TANK-Y (- BGH (/ (image-height TANK) 2)))

(place-image
 UFO 100 50
 (place-image
  TANK 150 TANK-Y
  BG))
