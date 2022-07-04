;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 376-count-hellos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An Xexpr.v2 is a list:
; - (cons Symbol Body)
; - (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XItem.v2 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))

; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; An XWord is '(word ((text String))).

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> [List-of Xexpr.v2]
; extract the list of content elements
(define (xexpr-content xe)
  (local ((define optional-loa+body (rest xe)))
    (cond
      [(empty? optional-loa+body) '()]
      [(list-of-attributes? (first optional-loa+body)) (rest optional-loa+body)]
      [else optional-loa+body])))

; Any -> Boolean
; return #true if x is an XWord
(define (word? x)
  (match x
    [`(word ((text ,str))) (string? str)]
    [x #false]))

; [List-of Attribute] Symbol -> [Maybe String]
; returns the value of an attribute s; #false if not found
(check-expect (find-attr '() 'foo) #false)
(check-expect (find-attr '((color "red")) 'color) "red")
(check-expect (find-attr '((color "red") (size "6px")) 'size) "6px")
(check-expect (find-attr '((color "red") (size "6px")) 'foo) #false)

(define (find-attr loa s)
  (local ((define attr (assq s loa)))
    (if (false? attr)
        #false
        (second attr))))

; XWord -> String
; extract the value of the text attribute of w
(define (word-text w)
  (find-attr (second w) 'text))

(define HELLO '(word ((text "hello"))))
(define WORLD '(word ((text "world"))))

(define UL1 '(ul))
(define UL1-COUNT 0)

(define UL2 `(ul ((id="1")) (li ,HELLO)))
(define UL2-COUNT 1)

(define UL3 `(ul (li ((size "10")) ,HELLO)))
(define UL3-COUNT 1)

(define UL4 `(ul (li ,WORLD)))
(define UL4-COUNT 0)

(define UL5 `(ul (li ((color="red")) ,HELLO)
                 (li ,WORLD)
                 (li ,UL3)
                 (li ,HELLO)))
(define UL5-COUNT 3)

(define UL6 `(ul
              (li ,UL5)
              (li ,HELLO)
              (li ,UL3)
              (li ,WORLD)
              (li ,UL4)
              (li ,UL2)))
(define UL6-COUNT 6)

; XItem.v2 -> N
; count all "hello"s in it
(check-expect (count-hellos-item `(li ,HELLO)) 1)
(check-expect (count-hellos-item `(li ((id="foo")) ,HELLO)) 1)
(check-expect (count-hellos-item `(li () ,HELLO)) 1)
(check-expect (count-hellos-item `(li ,WORLD)) 0)
(check-expect (count-hellos-item `(li ,UL6)) UL6-COUNT)
(check-expect (count-hellos-item `(li ((color="blue")) ,UL5)) UL5-COUNT)

(define (count-hellos-item it)
  (local (; [List-of Xexpr.v2]
          (define content (xexpr-content it))
          ; XWord or XEnum.v2
          (define word-or-enum (first content)))
    (cond
      [(word? word-or-enum) (if (string=? (word-text word-or-enum) "hello")
                           1
                           0)]
      [else (count-hellos word-or-enum)])))

; XEnum.v2 -> N
; count all "hello"s in xe
(check-expect (count-hellos UL1) UL1-COUNT)
(check-expect (count-hellos UL2) UL2-COUNT)
(check-expect (count-hellos UL3) UL3-COUNT)
(check-expect (count-hellos UL4) UL4-COUNT)
(check-expect (count-hellos UL5) UL5-COUNT)
(check-expect (count-hellos UL6) UL6-COUNT)

(define (count-hellos xe)
  (foldr
   ; XItem.v2 N -> N
   (lambda (it cnt)
     (+ cnt (count-hellos-item it)))
   0
   ; [List-of XItem.v2]
   (xexpr-content xe)))
