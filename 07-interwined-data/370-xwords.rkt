;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 370-xwords) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An XWord is '(word ((text String))).

(define XW1 '(word ((text "Hello, World!"))))
(define XW2 '(word ((text "Example #2"))))
(define XW3 '(word ((text "foo"))))

; Any -> Boolean
; return #true if x is an XWord
(check-expect (word? '(word ((text "hello")))) #true)
(check-expect (word? '(word ((text 123)))) #false)
(check-expect (word? '(word)) #false)
(check-expect (word? '(word ())) #false)
(check-expect (word? '(word ((hello "world")))) #false)
(check-expect (word? '(word ((text "hello") (foo "bar")))) #false)
(check-expect (word? '(foo ((text "hello")))) #false)
(check-expect (word? '()) #false)
(check-expect (word? "test") #false)
(check-expect (word? 123) #false)
(check-expect (word? #true) #false)

(define (word? x)
  (match x
    [`(word ((text ,str))) (string? str)]
    [x #false]))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; [List-of Attribute] Symbol -> [Maybe String]
; returns the value of an attribute s; #false if not found
(check-expect (find-attr '() 'foo) #false)
(check-expect (find-attr '((color "red")) 'color) "red")
(check-expect (find-attr '((color "red") (size "6px")) 'size) "6px")
(check-expect (find-attr '((color "red") (size "6px")) 'foo) #false)

(define (find-attr loa s)
  (local ((define attr (assq s loa)))
    (if (false? attr)
        #false
        (second attr))))

; XWord -> String
; extract the value of the text attribute of w
(check-expect (word-text '(word ((text "hello")))) "hello")
(check-expect (word-text '(word ((text "1 2")))) "1 2")

(define (word-text w)
  (find-attr (second w) 'text))
