;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |374|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)

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

; Any -> Boolean
; return #true if x is an XWord
(check-expect (word? '(word ((text "hello")))) #true)
(check-expect (word? '(word ((text 123)))) #false)
(check-expect (word? '(word)) #false)
(check-expect (word? '(word ())) #false)
(check-expect (word? '(word ((hello "world")))) #false)
(check-expect (word? '(word ((text "hello") (foo "bar")))) #false)
(check-expect (word? '(foo ((text "hello")))) #false)
(check-expect (word? '()) #false)
(check-expect (word? "test") #false)
(check-expect (word? 123) #false)
(check-expect (word? #true) #false)

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
(check-expect (word-text '(word ((text "hello")))) "hello")
(check-expect (word-text '(word ((text "1 2")))) "1 2")

(define (word-text w)
  (find-attr (second w) 'text))

; An XItem.v2 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))

; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid COLOR) (text " " SIZE COLOR)))

; Image -> Image
; marks item with bullet
(check-expect (bulletize (text "hello" SIZE COLOR))
              (beside/align 'center BT (text "hello" SIZE COLOR)))

(define (bulletize item)
  (beside/align 'center BT item))

(define UL1 '(ul
              (li (word ((text "hello"))))
              (li (word ((text "world"))))))
(define UL1-RENDERED (above/align 'left
                                  (beside/align 'center BT (text "hello" SIZE COLOR))
                                  (beside/align 'center BT (text "world" SIZE COLOR))))
(define UL2 `(ul
              (li (word ((text "foo"))))
              (li ,UL1)
              (li ((id="1")) (word ((text "bar"))))))
(define UL2-RENDERED (above/align 'left
                                  (beside/align 'center BT (text "foo" SIZE COLOR))
                                  (beside/align 'center BT UL1-RENDERED)
                                  (beside/align 'center BT (text "bar" SIZE COLOR))))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum UL1) UL1-RENDERED)
(check-expect (render-enum UL2) UL2-RENDERED)

(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item '(li (word ((text "hello")))))
              (beside/align 'center BT (text "hello" SIZE COLOR)))

(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE COLOR)]
       [else (render-enum content)]))))
