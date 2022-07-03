;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 362-interpreter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name expr])
; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

(define-struct con-def [name expr])
; BSL-con-def is a structure:
;   (make-con-def Symbol BSL-fun-expr)
; interpretation: constant definition

(define-struct fun-def [name param body])
; BSL-fun-def is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation: (make-fun-def n p b) represents a function definition with name n,
; parameter p and body b

; BSL-da-all is one of:
; - '()
; - (cons BSL-con-def BSL-da-all)
; - (cons BSL-fun-def BSL-da-all)
; interpretation: represents a definitions area with constant and function definitions

; BSL-def is one of:
; - BSL-con-def
; - BSL-fun-def

; (define close-to-pi 3.14)
(define close-to-pi-def
  (make-con-def 'close-to-pi 3.14))

; (define (area-of-circle r)
;   (* close-to-pi (* r r)))
(define area-of-circle-def
  (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))

; (define (volume-of-10-cylinder r)
;   (* 10 (area-of-circle r)))
(define volume-of-10-cylinder-def
  (make-fun-def 'volume-of-10-cylinder 'r (make-mul 10 (make-fun 'area-of-circle 'r))))

; BSL-da-all
(define DA1 (list close-to-pi-def
                  area-of-circle-def
                  volume-of-10-cylinder-def))

(define UNDEF-VAR "undefined variable")
(define UNDEF-FUN "undefined function")

; BSL-da-all Symbol -> BSL-con-def
; produces the representation of a constant definition whose name is x,
; if such a piece of data exists in da
(check-expect (lookup-con-def DA1 'close-to-pi) close-to-pi-def)
(check-error (lookup-con-def DA1 'area-of-circle) UNDEF-VAR)
(check-error (lookup-con-def '() 'close-to-pi) UNDEF-VAR)

(define (lookup-con-def da x)
  (cond
    [(empty? da) (error UNDEF-VAR)]
    [else (if (and (con-def? (first da))
                   (symbol=? (con-def-name (first da)) x))
              (first da)
              (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> BSL-fun-def
; produces the representation of a function definition whose name is f,
; if such a piece of data exists in da
(check-expect (lookup-fun-def DA1 'area-of-circle) area-of-circle-def)
(check-expect (lookup-fun-def DA1 'volume-of-10-cylinder) volume-of-10-cylinder-def)
(check-error (lookup-fun-def DA1 'close-to-pi) UNDEF-FUN)
(check-error (lookup-fun-def '() 'area-of-circle) UNDEF-FUN)

(define (lookup-fun-def da f)
  (cond
    [(empty? da) (error UNDEF-FUN)]
    [else (if (and (fun-def? (first da))
                   (symbol=? (fun-def-name (first da)) f))
              (first da)
              (lookup-fun-def (rest da) f))]))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; replace all occurrences of x by v in ex
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (subst (fun-name ex) x v) (subst (fun-expr ex) x v))]))

; BSL-fun-expr BSL-da-all -> Number
; evaluate an expression with the given definitions area
(check-expect (eval-all 123 '()) 123)
(check-error (eval-all 'x '()) UNDEF-VAR)
(check-expect (eval-all (make-add 1 2) '()) 3)
(check-expect (eval-all (make-mul 3 4) '()) 12)
(check-expect (eval-all (make-fun 'my-sqr 3)
                        (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
              9)
(check-expect (eval-all (make-fun 'my-sqr (make-add 1 2))
                        (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
              9)
(check-expect (eval-all (make-fun 'my-sqr (make-fun 'my-sqr 2))
                        (list (make-fun-def 'my-sqr 'x (make-mul 'x 'x))))
              16)
(check-expect (eval-all (make-fun 'foo 10)
                        (list (make-fun-def 'foo 'x 99)))
              99)
(check-error (eval-all (make-fun 'foo 10)
                       (list (make-fun-def 'bar 'x 99)))
             UNDEF-FUN)
(check-error (eval-all (make-fun 'foo 3)
                       (list (make-fun-def 'foo 'x (make-mul 'x 'y))))
             UNDEF-VAR)
(check-expect (eval-all (make-add (make-fun 'my-add1 5) 4)
                        (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
              10)
(check-expect (eval-all (make-mul (make-fun 'my-add1 5) 4)
                        (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
              24)
(check-expect (eval-all (make-fun 'foo 3)
                        (list (make-fun-def 'foo 'x (make-mul (make-add 'x 1) (make-mul 'x 'x)))))
              (* (+ 3 1) (* 3 3)))
(check-expect (eval-all (make-add (make-mul 2 3) (make-mul 1 2)) '()) 8)
(check-expect (eval-all (make-mul (make-mul 2 3) (make-mul 1 2)) '()) 12)
(check-error (eval-all (make-fun 'f 1)
                       (list (make-fun-def 'f 'x (make-fun 'g 'x))))
             UNDEF-FUN)
(check-expect (eval-all (make-add (make-fun 'f 3) (make-fun 'g 4))
                        (list (make-fun-def 'f 'x (make-fun 'h 'x))
                              (make-fun-def 'g 'y (make-mul 'y 'y))
                              (make-fun-def 'h 'z (make-add 'z 1))))
              (+ 4 16))
(check-expect (eval-all (make-fun 'volume-of-10-cylinder 5) DA1)
              (* 10 (* 3.14 (* 5 5))))
(check-expect (eval-all (make-add 'x 1)
                        (list (make-con-def 'x (make-fun 'f 3))
                              (make-fun-def 'f 'y (make-mul 'y 'y))))
              (+ (* 3 3) 1))

(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (local ((define cdef (lookup-con-def da ex)))
                    (eval-all (con-def-expr cdef) da))]
    [(add? ex) (+ (eval-all (add-left ex) da)
                  (eval-all (add-right ex) da))]
    [(mul? ex) (* (eval-all (mul-left ex) da)
                  (eval-all (mul-right ex) da))]
    [(fun? ex) (local ((define arg-val (eval-all (fun-expr ex) da))
                       (define fdef (lookup-fun-def da (fun-name ex)))
                       (define fbody-after-subst (subst (fun-def-body fdef) (fun-def-param fdef) arg-val)))
                 (eval-all fbody-after-subst da))]))

; Any -> Boolean
; return #true if x is an Atom
(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

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

; A Func-header is (list Symbol Symbol)

(define SL1 '((define close-to-pi 3.14)
              (define (area-of-circle r)
                (* close-to-pi (* r r)))
              (define (volume-of-10-cylinder r)
                (* 10 (area-of-circle r)))))

(define STRINGS-NOT-SUPPORTED "strings are not supported")
(define UNSUPPORTED-SL "unsupported SL")
(define PARSE-DEFS-ERROR "parse-defs error")

; Atom -> BSL-fun-expr
; parse Atom into BSL-expression
(check-expect (parse-atom 123) 123)
(check-expect (parse-atom 'foo) 'foo)
(check-error (parse-atom "foo") STRINGS-NOT-SUPPORTED)

(define (parse-atom at)
  (cond
    [(number? at) at]
    [(string? at) (error STRINGS-NOT-SUPPORTED)]
    [(symbol? at) at]))

; SL -> BSL-fun-expr
; parse SL into BSL-expression
(check-expect (parse-sl '(+ 1 2)) (make-add 1 2))
(check-expect (parse-sl '(* 3 4)) (make-mul 3 4))
(check-expect (parse-sl '(my-sqr 3)) (make-fun 'my-sqr 3))
(check-expect (parse-sl '(my-sqr (+ 1 2))) (make-fun 'my-sqr (make-add 1 2)))
(check-expect (parse-sl '(my-sqr (my-sqr 2))) (make-fun 'my-sqr (make-fun 'my-sqr 2)))
(check-expect (parse-sl '(+ (my-add1 5) 4)) (make-add (make-fun 'my-add1 5) 4))
(check-expect (parse-sl '(* (my-add1 5) 4)) (make-mul (make-fun 'my-add1 5) 4))
(check-expect (parse-sl '(+ (* 2 3) (* 1 2))) (make-add (make-mul 2 3) (make-mul 1 2)))
(check-expect (parse-sl '(* (* 2 3) (* 1 2))) (make-mul (make-mul 2 3) (make-mul 1 2)))
(check-expect (parse-sl '(+ (f 3) (g 4))) (make-add (make-fun 'f 3) (make-fun 'g 4)))
(check-expect (parse-sl '(volume-of-10-cylinder 5)) (make-fun 'volume-of-10-cylinder 5))
(check-expect (parse-sl '(+ x 1)) (make-add 'x 1))
(check-error (parse-sl '()) UNSUPPORTED-SL)
(check-error (parse-sl '(+ 1 2 3)) UNSUPPORTED-SL)
(check-error (parse-sl '(* 1 2 3)) UNSUPPORTED-SL)
(check-error (parse-sl '(foo 3 4)) UNSUPPORTED-SL)
(check-error (parse-sl '(1)) UNSUPPORTED-SL)

(define (parse-sl sl)
  (cond
    [(empty? sl) (error UNSUPPORTED-SL)]
    [else
     (cond
       [(symbol? (first sl))
        (cond
          [(symbol=? (first sl) '+) (if (= (length sl) 3)
                                        (make-add (parse-expr (second sl))
                                                  (parse-expr (third sl)))
                                        (error UNSUPPORTED-SL))]
          [(symbol=? (first sl) '*) (if (= (length sl) 3)
                                        (make-mul (parse-expr (second sl))
                                                  (parse-expr (third sl)))
                                        (error UNSUPPORTED-SL))]
          ; function application
          [else
           (if (= (length sl) 2)
               (make-fun (first sl) (parse-expr (second sl)))
               (error UNSUPPORTED-SL))])]
       [else (error UNSUPPORTED-SL)])]))

; S-expr -> BSL-fun-expr
; parse S-expression into BSL-expression
(check-expect (parse-expr 123) 123)
(check-expect (parse-expr 'x) 'x)
(check-expect (parse-expr '(+ 1 2)) (make-add 1 2))
(check-expect (parse-expr '(* 3 4)) (make-mul 3 4))
(check-expect (parse-expr '(my-sqr 3)) (make-fun 'my-sqr 3))
(check-expect (parse-expr '(my-sqr (+ 1 2))) (make-fun 'my-sqr (make-add 1 2)))
(check-expect (parse-expr '(my-sqr (my-sqr 2))) (make-fun 'my-sqr (make-fun 'my-sqr 2)))
(check-expect (parse-expr '(+ (my-add1 5) 4)) (make-add (make-fun 'my-add1 5) 4))
(check-expect (parse-expr '(* (my-add1 5) 4)) (make-mul (make-fun 'my-add1 5) 4))
(check-expect (parse-expr '(+ (* 2 3) (* 1 2))) (make-add (make-mul 2 3) (make-mul 1 2)))
(check-expect (parse-expr '(* (* 2 3) (* 1 2))) (make-mul (make-mul 2 3) (make-mul 1 2)))
(check-expect (parse-expr '(+ (f 3) (g 4))) (make-add (make-fun 'f 3) (make-fun 'g 4)))
(check-expect (parse-expr '(volume-of-10-cylinder 5)) (make-fun 'volume-of-10-cylinder 5))
(check-expect (parse-expr '(+ x 1)) (make-add 'x 1))

(define (parse-expr ex)
  (cond
    [(atom? ex) (parse-atom ex)]
    [else (parse-sl ex)]))

; Func-header S-expr -> BSL-fun-def
; parse a function definition with header h and body b
(define (parse-fun h b)
  (make-fun-def (first h) (second h) (parse-expr b)))

; Symbol S-expr -> BSL-con-def
; parse a constant definition with name n and body b
(define (parse-con n b)
  (make-con-def n (parse-expr b)))

; SL -> BSL-def
; parse a single definition
(check-expect (parse-def '(define (my-sqr arg) (* arg arg)))
              (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg)))
(check-expect (parse-def '(define (my-sqr x) (* x x)))
              (make-fun-def 'my-sqr 'x (make-mul 'x 'x)))
(check-expect (parse-def '(define (foo x) 99))
              (make-fun-def 'foo 'x 99))
(check-expect (parse-def '(define (foo x) (* x y)))
              (make-fun-def 'foo 'x (make-mul 'x 'y)))
(check-expect (parse-def '(define (my-add1 x) (+ x 1)))
              (make-fun-def 'my-add1 'x (make-add 'x 1)))
(check-expect (parse-def '(define (foo x) (* (+ x 1) (* x x))))
              (make-fun-def 'foo 'x (make-mul (make-add 'x 1) (make-mul 'x 'x))))
(check-expect (parse-def '(define (f x) (g x)))
              (make-fun-def 'f 'x (make-fun 'g 'x)))
(check-expect (parse-def '(define (f x) (h x)))
              (make-fun-def 'f 'x (make-fun 'h 'x)))
(check-expect (parse-def '(define (g y) (* y y)))
              (make-fun-def 'g 'y (make-mul 'y 'y)))
(check-expect (parse-def '(define (h z) (+ z 1)))
              (make-fun-def 'h 'z (make-add 'z 1)))
(check-expect (parse-def '(define close-to-pi 3.14))
              (make-con-def 'close-to-pi 3.14))
(check-expect (parse-def '(define (area-of-circle r)
                            (* close-to-pi (* r r))))
              (make-fun-def 'area-of-circle 'r
                            (make-mul 'close-to-pi (make-mul 'r 'r))))
(check-expect (parse-def '(define (volume-of-10-cylinder r)
                            (* 10 (area-of-circle r))))
              (make-fun-def 'volume-of-10-cylinder 'r
                            (make-mul 10 (make-fun 'area-of-circle 'r))))
(check-expect (parse-def '(define x (f 3)))
              (make-con-def 'x (make-fun 'f 3)))
(check-expect (parse-def '(define (f y) (* y y)))
              (make-fun-def 'f 'y (make-mul 'y 'y)))
(check-error (parse-def '(define x y z)) PARSE-DEFS-ERROR)
(check-error (parse-def '(foo 1)) PARSE-DEFS-ERROR)
(check-error (parse-def '(define 1 2)) PARSE-DEFS-ERROR)

(define (parse-def sl)
  (if (and (= (length sl) 3)
           (equal? (first sl) 'define))
      (cond
       [(cons? (second sl)) (parse-fun (second sl) (third sl))]
       [(symbol? (second sl)) (parse-con (second sl) (third sl))]
       [else (error PARSE-DEFS-ERROR)])
      (error PARSE-DEFS-ERROR)))

; SL -> BSL-da-all
; parse SL into BSL definitions area
(check-expect (parse-defs '()) '())
(check-expect (parse-defs '((define (my-sqr arg) (* arg arg))))
              (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
(check-expect (parse-defs '((define (my-sqr x) (* x x))))
              (list (make-fun-def 'my-sqr 'x (make-mul 'x 'x))))
(check-expect (parse-defs '((define (foo x) 99)))
              (list (make-fun-def 'foo 'x 99)))
(check-expect (parse-defs '((define (foo x) (* x y))))
              (list (make-fun-def 'foo 'x (make-mul 'x 'y))))
(check-expect (parse-defs '((define (my-add1 x) (+ x 1))))
              (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
(check-expect (parse-defs '((define (foo x) (* (+ x 1) (* x x)))))
              (list (make-fun-def 'foo 'x (make-mul (make-add 'x 1) (make-mul 'x 'x)))))
(check-expect (parse-defs '((define (f x) (g x))))
              (list (make-fun-def 'f 'x (make-fun 'g 'x))))
(check-expect (parse-defs '((define (f x) (h x))
                            (define (g y) (* y y))
                            (define (h z) (+ z 1))))
              (list (make-fun-def 'f 'x (make-fun 'h 'x))
                    (make-fun-def 'g 'y (make-mul 'y 'y))
                    (make-fun-def 'h 'z (make-add 'z 1))))
(check-expect (parse-defs SL1) DA1)
(check-expect (parse-defs '((define x (f 3))
                            (define (f y) (* y y))))
              (list (make-con-def 'x (make-fun 'f 3))
                    (make-fun-def 'f 'y (make-mul 'y 'y))))

(define (parse-defs sl)
  (map parse-def sl))

; S-expr SL -> Number
; parse and evaluate an S-expression ex using a list of definitions sl
(check-expect (interpreter 123 '()) 123)
(check-error (interpreter 'x '()) UNDEF-VAR)
(check-expect (interpreter '(+ 1 2) '()) 3)
(check-expect (interpreter '(* 3 4) '()) 12)
(check-expect (interpreter '(my-sqr 3)
                           '((define (my-sqr arg) (* arg arg))))
              9)
(check-expect (interpreter '(my-sqr (+ 1 2))
                           '((define (my-sqr arg) (* arg arg))))
              9)
(check-expect (interpreter '(my-sqr (my-sqr 2))
                           '((define (my-sqr x) (* x x))))
              16)
(check-expect (interpreter '(foo 10)
                           '((define (foo x) 99)))
              99)
(check-error (interpreter '(foo 10)
                          '((define (bar x) 99)))
             UNDEF-FUN)
(check-error (interpreter '(foo 3)
                          '((define (foo x) (* x y))))
             UNDEF-VAR)
(check-expect (interpreter '(+ (my-add1 5) 4)
                           '((define (my-add1 x) (+ x 1))))
              10)
(check-expect (interpreter '(* (my-add1 5) 4)
                           '((define (my-add1 x) (+ x 1))))
              24)
(check-expect (interpreter '(foo 3)
                           '((define (foo x) (* (+ x 1) (* x x)))))
              (* (+ 3 1) (* 3 3)))
(check-expect (interpreter '(+ (* 2 3) (* 1 2)) '()) 8)
(check-expect (interpreter '(* (* 2 3) (* 1 2)) '()) 12)
(check-error (interpreter '(f 1)
                          '((define (f x) (g x))))
             UNDEF-FUN)
(check-expect (interpreter '(+ (f 3) (g 4))
                           '((define (f x) (h x))
                             (define (g y) (* y y))
                             (define (h z) (+ z 1))))
              (+ 4 16))
(check-expect (interpreter '(volume-of-10-cylinder 5) SL1)
              (* 10 (* 3.14 (* 5 5))))
(check-expect (interpreter '(+ x 1)
                           '((define x (f 3))
                             (define (f y) (* y y))))
              (+ (* 3 3) 1))

(define (interpreter ex sl)
  (eval-all (parse-expr ex)
            (parse-defs sl)))
