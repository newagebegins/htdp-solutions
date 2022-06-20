;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |017|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define (image-classify img)
  (cond
    [(= (image-width img) (image-height img)) "square"]
    [(> (image-width img) (image-height img)) "wide"]
    [else "tall"]))

(image-classify (rectangle 10 10 "solid" "blue"))
(image-classify (rectangle 20 10 "solid" "blue"))
(image-classify (rectangle 10 20 "solid" "blue"))
