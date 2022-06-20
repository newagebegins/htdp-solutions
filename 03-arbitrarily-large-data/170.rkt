;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |170|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999.
; A Four is a Number between 1000 and 9999.

; A List-of-phones is one of:
; - '()
; - (cons Phone List-of-phones)

; Phone -> Phone
; replace area code 713 with 281
(check-expect (replace1 (make-phone 713 100 1000)) (make-phone 281 100 1000))
(check-expect (replace1 (make-phone 555 100 1000)) (make-phone 555 100 1000))

(define (replace1 p)
  (make-phone (if (= (phone-area p) 713)
                  281
                  (phone-area p))
              (phone-switch p)
              (phone-four p)))

; List-of-phones -> List-of-phones
; replace all occurences of area code 713 with 281
(check-expect (replace '()) '())
(check-expect (replace (cons (make-phone 713 100 1000) '())) (cons (make-phone 281 100 1000) '()))
(check-expect (replace (cons (make-phone 555 100 1000) '())) (cons (make-phone 555 100 1000) '()))
(check-expect (replace (cons (make-phone 713 456 7890) (cons (make-phone 713 100 1000) '())))
              (cons (make-phone 281 456 7890) (cons (make-phone 281 100 1000) '())))
(check-expect (replace (cons (make-phone 222 333 9999)
                             (cons (make-phone 713 456 7890)
                                   (cons (make-phone 777 100 1000) '()))))
              (cons (make-phone 222 333 9999)
                             (cons (make-phone 281 456 7890)
                                   (cons (make-phone 777 100 1000) '()))))

(define (replace l)
  (cond
    [(empty? l) '()]
    [else (cons (replace1 (first l)) (replace (rest l)))]))
