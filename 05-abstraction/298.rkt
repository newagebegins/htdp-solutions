;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |298|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An ImageStream is a function:
;   [N -> Image]
; interpretation: a stream s denotes a series of images

(define ROCKET (bitmap "../assets/rocket.png"))

; ImageStream
(define (create-rocket-scene height)
  (place-image ROCKET 30 height (empty-scene 60 60)))

; [N -> Image] N -> N
; show the images (s 0), (s 1), and so on at a rate of 30 images per second up to n images total
(define (my-animate s n)
  (big-bang 0
    [to-draw s]
    [on-tick add1 1/30 n]))
