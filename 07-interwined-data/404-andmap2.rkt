;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 404-andmap2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Any Any -> Boolean] [List-of Any] [List-of Any] -> Boolean
; like andmap but for processing two equally long lists simultaniously
(check-expect (andmap2 (lambda (x y) (x y))
                       (list boolean? string? integer?)
                       (list #false "hello" 8))
              #true)
(check-expect (andmap2 (lambda (x y) (x y))
                       (list boolean? string? integer?)
                       (list #false 'hello 8))
              #false)

(define (andmap2 f l1 l2)
  (cond
    [(empty? l1) #true]
    [else (and (f (first l1) (first l2))
               (andmap2 f (rest l1) (rest l2)))]))
