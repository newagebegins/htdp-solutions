;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |142|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ListOfImages is one of:
; - '()
; - (cons Image ListOfImages)

; ImageOrFalse is one of:
; – Image
; – #false

; ListOfImages PositiveNumber -> ImageOrFalse
; produces the first image on loi that is not an n by n square
; if it cannot find such an image, it produces #false.
(check-expect (ill-sized? '() 5) #false)
(check-expect (ill-sized? (cons (square 3 "solid" "black") '()) 5) (square 3 "solid" "black"))
(check-expect (ill-sized? (cons (square 3 "solid" "black") '()) 3) #false)
(check-expect (ill-sized? (cons (circle 3 "solid" "black") '()) 5) (circle 3 "solid" "black"))
(check-expect (ill-sized? (cons (circle 3 "solid" "black") '()) 6) #false)
(check-expect (ill-sized? (cons (square 7 "solid" "black")
                                (cons (square 5 "solid" "green") '())) 7) (square 5 "solid" "green"))

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (if (and (= (image-width (first loi)) n)
                   (= (image-height (first loi)) n))
              (ill-sized? (rest loi) n)
              (first loi))]))
