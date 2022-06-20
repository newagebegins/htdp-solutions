;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |008|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define cat (bitmap "../assets/cat.png"))
;(define cat (rectangle 20 20 "solid" "black"))
(if (>= (image-height cat) (image-width cat)) "tall" "wide")
(if (= (image-height cat) (image-width cat)) "square"
    (if (> (image-height cat) (image-width cat)) "tall" "wide"))
