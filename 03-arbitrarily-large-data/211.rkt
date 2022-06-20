;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |211|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; On OS X: 
(define LOCATION "/usr/share/dict/words")
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "cat")) (list "cat"))
(check-expect (in-dictionary (list "asdf")) '())
(check-expect (in-dictionary (list "asdf" "ww")) '())
(check-expect (in-dictionary (list "cat" "asdf" "dog")) (list "cat" "dog"))
(check-expect (in-dictionary (list "ww" "cat" "asdf" "dog" "asdfww")) (list "cat" "dog"))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) AS-LIST)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))
