;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |289|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String [List-of String] -> Boolean
; determines whether any of the names in l are equal to or an extension of the name n
(check-expect (find-name "John" '()) #false)
(check-expect (find-name "John" '("John")) #true)
(check-expect (find-name "Johnny" '("John")) #false)
(check-expect (find-name "John" '("John Smith")) #true)
(check-expect (find-name "John" '("Bob")) #false)
(check-expect (find-name "John" '("Bob" "John" "Michael")) #true)
(check-expect (find-name "John" '("Bob" "Michael")) #false)
(check-expect (find-name "Michael" '("Bob" "John" "Michael")) #true)
(check-expect (find-name "Bob" '("John" "Bobby" "Michael" "Bobster")) #true)

(define (find-name n l)
  (ormap (lambda (x) (string-contains? n x)) l))
