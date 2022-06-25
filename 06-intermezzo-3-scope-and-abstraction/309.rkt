;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |309|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of N]
; determines the number of strings per item in a list of list of strings
(check-expect (words-on-line '()) '())
(check-expect (words-on-line '(("lorem" "ipsum")
                               ("dolor" "sit" "amet")
                               ()
                               ("hello")))
              '(2 3 0 1))

(define (words-on-line lines)
  (for/list ([l lines])
    (length l)))
