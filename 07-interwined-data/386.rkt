;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |386|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/abstraction)

(define MOCK-STOCKS (read-xexpr "../assets/stock-quotes.html"))

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

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr -> [Maybe [List-of Attribute]]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr '(foo ((atr1 "val1") (atr2 "val2")) (bar))) '((atr1 "val1") (atr2 "val2")))
(check-expect (xexpr-attr '(foo ())) '())
(check-expect (xexpr-attr '(foo)) #false)
(check-expect (xexpr-attr '(foo (bar))) #false)

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

; Xexpr -> Symbol
; extract the tag of the element representation
(define (xexpr-name xe)
  (first xe))

; Xexpr -> [List-of Xexpr]
; extract the list of content elements
(define (xexpr-content xe)
  (local ((define optional-loa+body (rest xe)))
    (cond
      [(empty? optional-loa+body) '()]
      [(list-of-attributes? (first optional-loa+body)) (rest optional-loa+body)]
      [else optional-loa+body])))

; Xexpr String -> [Maybe String]
; retrieves the value of the "content" attribute
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect (get-xexpr 'foo "F") #false)
(check-expect (get-xexpr "foo" "F") #false)
(check-expect (get-xexpr 123 "F") #false)
(check-expect (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F") "+1")
(check-expect (get-xexpr '(meta ((content "17.09") (itemprop "price"))) "price") "17.09")
(check-expect (get-xexpr '(meta ((content "+1") (itemprop "F"))) "price") #false)
(check-expect (get-xexpr MOCK-STOCKS "price") "17.09")
(check-expect (get-xexpr MOCK-STOCKS "priceChange") "+0.07")
(check-expect (get-xexpr MOCK-STOCKS "foo") #false)

(define (get-xexpr x0 s)
  (cond
    [(symbol? x0) #false]
    [(string? x0) #false]
    [(number? x0) #false]
    [else ; cons
     (if (symbol=? (xexpr-name x0) 'meta)
         (local ((define attr (xexpr-attr x0)))
           (if (equal? (find-attr attr 'itemprop) s)
               (find-attr attr 'content)
               #false))
         (for/or ([x (xexpr-content x0)])
           (get-xexpr x s)))]))

; Xexpr String -> String
; retrieves the value of the "content" attribute
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect (get '(meta ((content "+1") (itemprop "F"))) "F") "+1")
(check-expect (get '(meta ((content "17.09") (itemprop "price"))) "price") "17.09")
(check-error (get '(meta ((content "+1") (itemprop "F"))) "price") "not found")

(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))
