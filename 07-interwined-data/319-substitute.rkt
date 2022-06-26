;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 319-substitute) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of:
; – Atom
; – SL

; An Atom is one of:
; – Number
; – String
; – Symbol

; An SL is one of:
; – '()
; – (cons S-expr SL)

; Any -> Boolean
; return #true if x is an Atom
(check-expect (atom? 7) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'hello) #true)
(check-expect (atom? '()) #false)

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; S-expr Symbol Symbol -> S-expr
; replaces old with new in s
(check-expect (substitute 'foo 'foo 'bar) 'bar)
(check-expect (substitute 'hello 'foo 'bar) 'hello)
(check-expect (substitute '() 'a 'b) '())
(check-expect (substitute 3 'a 'b) 3)
(check-expect (substitute "hello" 'a 'b) "hello")
(check-expect (substitute '(foo) 'foo 'bar) '(bar))
(check-expect (substitute '(hello) 'foo 'bar) '(hello))
(check-expect (substitute '(1 a b a ("c" (a 3 (d a)) a)) 'a 'b)
              '(1 b b b ("c" (b 3 (d b)) b)))

(define (substitute s old new)
  (cond
    [(atom? s) (if (and (symbol? s) (symbol=? s old))
                   new
                   s)]
    [else (map (lambda (x) (substitute x old new)) s)]))
