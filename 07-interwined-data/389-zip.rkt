;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 389-zip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combine equally long lists of names and phones into a list of phone records
; assume that corresponding list items belong to the same person
(check-expect (zip '() '()) '())
(check-expect (zip '("Alice" "Bob") '("123-456" "789-012"))
              (list (make-phone-record "Alice" "123-456")
                    (make-phone-record "Bob" "789-012")))

(define (zip names phones)
  (cond
    [(empty? names) '()]
    [else (cons (make-phone-record (first names) (first phones))
                (zip (rest names) (rest phones)))]))
