;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |308|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct phone [area switch four])

; [List-of Phone] -> [List-of Phone]
; substitutes the area code 713 with 281 for the phones in the list
(check-expect (replace '()) '())
(check-expect (replace (list (make-phone 713 664 9993)
                             (make-phone 713 664 9999)
                             (make-phone 281 660 9999)
                             (make-phone 333 660 9999)))
              (list (make-phone 281 664 9993)
                    (make-phone 281 664 9999)
                    (make-phone 281 660 9999)
                    (make-phone 333 660 9999)))

(define (replace lop)
  (for/list ([p lop])
    (match p
      [(phone 713 s f) (make-phone 281 s f)]
      [x x])))
