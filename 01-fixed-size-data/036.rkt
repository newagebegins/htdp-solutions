;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |036|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Image -> Number
;; count the number of pixels in an image
;; given: (rectangle 10 20 "solid" "blue"), expect: 200
(define (image-area img)
  (* (image-width img)
     (image-height img)))
