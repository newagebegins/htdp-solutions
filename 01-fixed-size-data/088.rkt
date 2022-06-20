;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |088|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Happiness is a Number[0, 100]

(define-struct v-cat [x happiness])
; VCat is (make-v-cat Number Happiness)
; interp. a cat at distance x from the left edge of the scene, with given happiness level
