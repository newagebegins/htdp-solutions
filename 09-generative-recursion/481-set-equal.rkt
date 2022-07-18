;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 481-set-equal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List List -> Boolean
; return #true if two lists contain the same items - regardless of order
; assume: items do not repeat
(check-expect (set=? '() '()) #true)
(check-expect (set=? '(1) '(1)) #true)
(check-expect (set=? '(1) '(2)) #false)
(check-expect (set=? '(1) '()) #false)
(check-expect (set=? '(1) '(1 2)) #false)
(check-expect (set=? '(1 2) '(2)) #false)
(check-expect (set=? '(1 2) '(2 1)) #true)
(check-expect (set=? '(a b) '(b a)) #true)
(check-expect (set=? '(1 2 3) '(2 1)) #false)
(check-expect (set=? (list (make-posn 0 0) (make-posn 0 3) (make-posn 3 5))
                     (list (make-posn 3 5) (make-posn 0 0) (make-posn 0 3)))
              #true)
(check-expect (set=? (list (make-posn 0 0) (make-posn 0 3) (make-posn 3 5))
                     (list (make-posn 3 5) (make-posn 0 1) (make-posn 0 3)))
              #false)
(check-expect (set=? '(1 2 3 4 5 6) '(2 1 6 4 3 5)) #true)
(check-expect (set=? '(1 2 3 4 5 6) '(2 1 6 4 3 5)) #true)

(define (set=? s1 s2)
  (local (; List List -> Boolean
          ; return #true if s1 is a subset of s2
          (define (subset? s1 s2)
            (andmap (lambda (x) (member? x s2)) s1)))
    (and (subset? s1 s2)
         (subset? s2 s1))))
