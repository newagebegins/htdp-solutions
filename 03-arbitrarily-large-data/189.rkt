;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |189|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of:
; - '()
; - (cons Numbers List-of-numbers)

; Number List-of-numbers -> Boolean
(check-expect (search-sorted 3 '()) #false)

(check-expect (search-sorted 3 (list 3)) #true)
(check-expect (search-sorted 3 (list 4)) #false)

(check-expect (search-sorted 3 (list 3 2)) #true)

(check-expect (search-sorted 4 (list 4 3 2)) #true)
(check-expect (search-sorted 3 (list 4 3 2)) #true)
(check-expect (search-sorted 2 (list 4 3 2)) #true)

(check-expect (search-sorted 5 (list 4 3 2)) #false)
(check-expect (search-sorted 1 (list 4 3 2)) #false)

(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [else
     (cond
       [(= n (first alon)) #true]
       [(> n (first alon)) #false]
       [(< n (first alon)) (search-sorted n (rest alon))])]))
