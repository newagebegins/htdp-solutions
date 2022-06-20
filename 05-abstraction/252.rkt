;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |252|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; [List-of Number] -> Number
(check-expect (product '(1 2 3 4)) 24)

(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))

; [List-of Posn] -> Image
(check-expect (image* (list (make-posn 1 2) (make-posn 3 4)))
              (place-dot (make-posn 1 2)
                         (place-dot (make-posn 3 4)
                                    emt)))

(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))

; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

; graphical constants:    
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

; [Any Any -> Any] Any [List-of Any]
(check-expect (fold2 * 1 '(1 2 3 4)) 24)
(check-expect (fold2 place-dot emt (list (make-posn 1 2) (make-posn 3 4)))
              (place-dot (make-posn 1 2)
                         (place-dot (make-posn 3 4)
                                    emt)))

(define (fold2 f d l)
  (cond
    [(empty? l) d]
    [else
     (f (first l)
        (fold2 f d (rest l)))]))
