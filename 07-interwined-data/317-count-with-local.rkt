;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 317-count-with-local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'hello 'hello) 1)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(check-expect (count '(((world "foo") hello 3) hello) 'hello) 2)

(define (count sexp sy)
  (local (; SL Symbol -> N
          ; counts all occurrences of sy in sl
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))

          ; Atom Symbol -> N
          ; counts all occurrences of sy in at
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))
