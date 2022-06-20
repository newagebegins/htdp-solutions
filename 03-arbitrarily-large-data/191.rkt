;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |191|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A NELoP (Not Empty List of Points) is one of:
; - (cons Posn '())
; - (cons Posn NELoP)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))

(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; a plain background image
(define MT (empty-scene 50 50))

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 20 20 10 20 "red")
                 20 10 20 20 "red")
               10 10 20 10 "red"))

(define (connect-dots img p)
  MT)
