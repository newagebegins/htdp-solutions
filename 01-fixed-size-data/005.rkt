;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |005|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define PIXELS_PER_UNIT 50)
(define (u units) (* units PIXELS_PER_UNIT))
(overlay/align
 "middle" "top"
 (rotate -90 (triangle (u 4) "solid" "green"))
 (overlay/align
  "right" "bottom"
  (triangle/ass 90 (u 2) (u 2) "solid" "brown")
  (overlay/align
   "left" "bottom"
   (rectangle (u 5) (u 2) "solid" "brown")
   (empty-scene (u 7) (u 6)))))
