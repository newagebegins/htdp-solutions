;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |307|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; String [List-of String] -> [Maybe String]
; retrieves the first name in l that is equal to or an extension of the name n
; returns #false if no such name was found
(check-expect (find-name "John" '()) #false)
(check-expect (find-name "John" '("John")) "John")
(check-expect (find-name "Johnny" '("John")) #false)
(check-expect (find-name "John" '("John Smith")) "John Smith")
(check-expect (find-name "John" '("Bob")) #false)
(check-expect (find-name "John" '("Bob" "John" "Michael")) "John")
(check-expect (find-name "John" '("Bob" "Michael")) #false)
(check-expect (find-name "Michael" '("Bob" "John" "Michael")) "Michael")
(check-expect (find-name "Bob" '("John" "Bobby" "Michael" "Bobster")) "Bobby")

(define (find-name n l)
  (for/or ([x l])
    (if (string-contains? n x) x #false)))

; N [List-of String] -> Boolean
; returns #true if at least one name on the list exceeds the given width
(check-expect (too-long? 1 '()) #false)
(check-expect (too-long? 1 '("a" "b" "c")) #false)
(check-expect (too-long? 1 '("a" "bb" "c")) #true)
(check-expect (too-long? 2 '("a" "bb" "c")) #false)
(check-expect (too-long? 3 '("aaa" "bb" "c")) #false)
(check-expect (too-long? 3 '("aaaa" "bb" "c")) #true)
(check-expect (too-long? 3 '("aaaa" "bb" "cccc")) #true)
(check-expect (too-long? 3 '("a" "bb" "cccc")) #true)

(define (too-long? w l)
  (for/or ([x l])
    (> (string-length x) w)))
