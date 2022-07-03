;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 366-xexpr-name-and-content) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr.v2 is a list:
; - (cons Symbol Body)
; - (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> Symbol
; extract the tag of the element representation
(check-expect (xexpr-name '(machine)) 'machine)
(check-expect (xexpr-name '(foo (bar))) 'foo)
(check-expect (xexpr-name '(foo ((a "b")) (bar))) 'foo)

(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> [List-of Xexpr.v2]
; extract the list of content elements
(check-expect (xexpr-content '(machine)) '())
(check-expect (xexpr-content '(machine ())) '())
(check-expect (xexpr-content '(machine ((foo "bar")))) '())
(check-expect (xexpr-content '(foo (bar))) '((bar)))
(check-expect (xexpr-content '(foo (bar) (baz))) '((bar) (baz)))
(check-expect (xexpr-content '(foo (bar (baz)))) '((bar (baz))))
(check-expect (xexpr-content '(foo () (bar))) '((bar)))
(check-expect (xexpr-content '(foo ((hello="world")) (bar))) '((bar)))

(define (xexpr-content xe)
  (local ((define optional-loa+body (rest xe)))
    (cond
      [(empty? optional-loa+body) '()]
      [(list-of-attributes? (first optional-loa+body)) (rest optional-loa+body)]
      [else optional-loa+body])))
