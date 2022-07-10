;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |429|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define THRESHOLD 2)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< '(11 8 14 7)) '(7 8 11 14))
(check-expect (quick-sort< '(7 11 8 14 7 8)) '(7 7 8 8 11 14))
(check-expect (quick-sort< '(1 1 1)) '(1 1 1))

(define (quick-sort< alon)
  (cond
    [(<= (length alon) 1) alon]
    [(<= (length alon) THRESHOLD) (sort alon <)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (filter (lambda (x) (= x pivot)) alon)
                    (quick-sort< (largers alon pivot))))]))

; [List-of Number] Number -> [List-of Number]
; produce a list of all the numbers in alon that are strictly larger than n
(define (largers alon n)
  (filter (lambda (x) (> x n)) alon))

; [List-of Number] Number -> [List-of Number]
; produce a list of all the numbers in alon that are strictly smaller than n
(define (smallers alon n)
  (filter (lambda (x) (< x n)) alon))
