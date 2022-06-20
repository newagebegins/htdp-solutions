;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |134|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; String List-of-strings -> Boolean
; determines whether a given string appears in a given list of strings
(check-expect (contains? "Flatt" '()) #false)
(check-expect (contains? "Flatt" (cons "Find" '())) #false)
(check-expect (contains? "Flatt" (cons "Flatt" '())) #true)
(check-expect (contains? "Flatt" (cons "A" (cons "Flatt" (cons "C" '())))) #true)
(check-expect (contains? "Flatt" (cons "A" (cons "Find" (cons "C" '())))) #false)

(define (contains? s alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) s)
         (contains? s (rest alon)))]))
