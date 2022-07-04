;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 377-replace-hellos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Xexpr.v2 -> [Maybe [List-of Attribute]]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr '(foo ((atr1 "val1") (atr2 "val2")) (bar))) '((atr1 "val1") (atr2 "val2")))
(check-expect (xexpr-attr '(foo ())) '())
(check-expect (xexpr-attr '(foo)) #false)

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) #false]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             #false))])))

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

(define HELLO '(word ((text "hello"))))
(define WORLD '(word ((text "world"))))
(define BYE '(word ((text "bye"))))

(define UL1 '(ul))
(define UL1-R UL1)

(define UL2 `(ul (li ,HELLO)))
(define UL2-R `(ul (li ,BYE)))

(define UL3 `(ul (li ,WORLD)))
(define UL3-R UL3)

(define UL4 `(ul () (li () ,HELLO)))
(define UL4-R `(ul () (li () ,BYE)))

(define UL5 `(ul (li ((color "green")) ,HELLO)))
(define UL5-R `(ul (li ((color "green")) ,BYE)))

(define UL6 `(ul ((id="1" font="big"))
                 (li ,HELLO)
                 (li ,UL2)
                 (li ,WORLD)
                 (li ,UL4)))
(define UL6-R `(ul ((id="1" font="big"))
                   (li ,BYE)
                   (li ,UL2-R)
                   (li ,WORLD)
                   (li ,UL4-R)))

; XItem.v2 -> XItem.v2
; replace all "hello"s with "bye"s in it
(check-expect (replace-hellos-item `(li ,HELLO)) `(li ,BYE))
(check-expect (replace-hellos-item `(li () ,HELLO)) `(li () ,BYE))
(check-expect (replace-hellos-item `(li ((color "green")) ,HELLO)) `(li ((color "green")) ,BYE))
(check-expect (replace-hellos-item `(li ,WORLD)) `(li ,WORLD))
(check-expect (replace-hellos-item `(li ,UL6)) `(li ,UL6-R))

(define (replace-hellos-item it)
  (local (; [Maybe [List-of Attribute]]
          (define attrs (xexpr-attr it))
          ; [List-of Xexpr.v2]
          (define content (xexpr-content it))
          ; XWord or XEnum.v2
          (define word-or-enum (first content))
          ; XWord or XEnum.v2 -> XWord or XEnum.v2
          (define (replace x)
            (cond
              [(word? x) (if (equal? x HELLO)
                             BYE
                             x)]
              [else (replace-hellos x)])))
    (cond
      [(false? attrs) (cons 'li (cons (replace word-or-enum) '()))]
      [else (cons 'li (cons attrs (cons (replace word-or-enum) '())))])))

; XEnum.v2 -> XEnum.v2
; replace all "hello"s with "bye"s in xe
(check-expect (replace-hellos UL1) UL1-R)
(check-expect (replace-hellos UL2) UL2-R)
(check-expect (replace-hellos UL3) UL3-R)
(check-expect (replace-hellos UL4) UL4-R)
(check-expect (replace-hellos UL5) UL5-R)
(check-expect (replace-hellos UL6) UL6-R)

(define (replace-hellos xe)
  (local (; [Maybe [List-of Attribute]]
          (define attrs (xexpr-attr xe))
          ; [List-of XItem.v2]
          (define items (xexpr-content xe))
          ; [List-of XItem.v2]
          (define replaced-items (map replace-hellos-item items)))
    (cond
      [(false? attrs) (cons 'ul replaced-items)]
      [else (cons 'ul (cons attrs replaced-items))])))
