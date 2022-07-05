;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 387-cross) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Symbol [List-of Number] -> [List-of (list Symbol Number)]
; produce ordered pairs of one symbol and numbers from the list
(check-expect (cross1 'a '()) '())
(check-expect (cross1 'a '(1 2)) '((a 1) (a 2)))
(check-expect (cross1 'b '(1 2 3)) '((b 1) (b 2) (b 3)))

(define (cross1 s lon)
  (cond
    [(empty? lon) '()]
    [else (cons (list s (first lon))
                (cross1 s (rest lon)))]))

; [List-of Symbol] [List-of Number] -> [List-of (list Symbol Number)]
; produce all possible ordered pairs of symbols and numbers from the two given lists
(check-expect (cross '() '()) '())
(check-expect (cross '(a b c) '()) '())
(check-expect (cross '() '(1 2)) '())
(check-expect (cross '(a) '(1)) '((a 1)))
(check-expect (cross '(a) '(1 2)) '((a 1) (a 2)))
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(check-expect (cross '(a b c) '(1 2 3)) '((a 1) (a 2) (a 3) (b 1) (b 2) (b 3) (c 1) (c 2) (c 3)))

(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else (append (cross1 (first los) lon)
                  (cross (rest los) lon))]))
