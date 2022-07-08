;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 409-reorder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint: cells do not contain functions

; integrity constraint: In (make-db sch con), for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema `(("Name"    ,string?)
                        ("Age"     ,integer?)
                        ("Present" ,boolean?)))
(define school-content '(("Alice" 35 #true)
                         ("Bob"   25 #false)
                         ("Carol" 30 #true)
                         ("Dave"  32 #false)))
(define school-db (make-db school-schema school-content))

(define labels1 '("Present" "Name" "Age"))
(define indices1 '(2 0 1))
(define schema1 `(("Present" ,boolean?)
                  ("Name" ,string?)
                  ("Age" ,integer?)))
(define content1 '((#true "Alice" 35)
                   (#false "Bob" 25)
                   (#true "Carol" 30)
                   (#false "Dave" 32)))

(define labels2 '("Age" "Name"))
(define indices2 '(1 0))
(define schema2 `(("Age" ,integer?)
                  ("Name" ,string?)))
(define content2 '((35 "Alice")
                   (25 "Bob")
                   (30 "Carol")
                   (32 "Dave")))

; Label Schema -> N
; produce the index of the spec with label l inside the schema s
(check-expect (spec-index "Name" school-schema) 0)
(check-expect (spec-index "Age" school-schema) 1)
(check-expect (spec-index "Present" school-schema) 2)
(check-error (spec-index "foo" school-schema) "spec foo not found")

(define (spec-index l s)
  (cond
    [(empty? s) (error "spec " l " not found")]
    [else (if (string=? (first (first s)) l)
              0
              (+ 1 (spec-index l (rest s))))]))

; Schema [List-of Label] -> [List-of N]
; produce a list of reordered indices of specs according to lol
(check-expect (reorder-indices school-schema labels1) indices1)

(define (reorder-indices s lol)
  (map (lambda (l) (spec-index l s)) lol))

; Schema [List-of N] -> Schema
; produce a schema like s but with its specs reordered according to the indices loi
(check-expect (map first (reorder-schema school-schema indices1))
              (map first schema1))

(define (reorder-schema s loi)
  (foldr (lambda (i res) (cons (list-ref s i) res)) '() loi))

; Content [List-of N] -> Content
; produce new content like c but with cells reordered according to indices loi
(check-expect (reorder-content school-content indices1) content1)

(define (reorder-content c loi)
  (local (; Row -> Row
          (define (reorder-row r)
            (foldr (lambda (i res) (cons (list-ref r i) res)) '() loi)))
    (map reorder-row c)))

; DB [List-of Label] -> DB
; produce a database like db but with its columns reordered according to lol
(check-expect (map first (db-schema (reorder school-db labels1)))
              (map first schema1))
(check-expect (db-content (reorder school-db labels1))
              content1)

(check-expect (map first (db-schema (reorder school-db labels2)))
              (map first schema2))
(check-expect (db-content (reorder school-db labels2))
              content2)

(check-error (reorder school-db '("Name" "Hello" "Present")) "spec Hello not found")

(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define indices (reorder-indices schema lol)))
    (make-db (reorder-schema schema indices)
             (reorder-content content indices))))
