;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |372|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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

; An XEnum.v1 is one of:
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid COLOR) (text " " SIZE COLOR)))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 '(li (word ((text "hello")))))
              (beside/align 'center BT (text "hello" SIZE COLOR)))
(check-expect (render-item1 '(li ((id "1")) (word ((text "world")))))
              (beside/align 'center BT (text "world" SIZE COLOR)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word SIZE COLOR)))
    (beside/align 'center BT item)))
