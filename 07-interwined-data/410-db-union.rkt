;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 410-db-union) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define S1 `(("Name"    ,string?)
             ("Age"     ,integer?)
             ("Present" ,boolean?)))

(define C1 '(("Alice" 35 #true)
             ("Bob"   25 #false)
             ("Carol" 30 #true)
             ("Dave"  32 #false)))
(define DB1 (make-db S1 C1))

(define C2 '(("Carol" 30 #true)
             ("Bernard" 40 #true)
             ("Alice" 35 #true)
             ("Mike"  22 #false)
             ("Larry" 30 #false)))
(define DB2 (make-db S1 C2))

(define C1+2 '(("Alice" 35 #true)
               ("Bob"   25 #false)
               ("Carol" 30 #true)
               ("Dave"  32 #false)
               ("Bernard" 40 #true)
               ("Mike"  22 #false)
               ("Larry" 30 #false)))

; [X] [List-of X] -> [List-of X]
; remove duplicate items in the list l
(check-expect (remove-duplicates '()) '())
(check-expect (remove-duplicates '(a b c a j b k)) '(a b c j k))

(define (remove-duplicates l)
  (reverse (foldl
            ; X [List-of X] -> [List-of X]
            (lambda (it res)
              (if (member? it res)
                  res
                  (cons it res)))
            '()
            l)))

; Content Content -> Content
; produce new content by joining c1 and c2, duplicate rows are eliminated
(check-expect (content-union C1 C2) C1+2)

(define (content-union c1 c2)
  (remove-duplicates (append c1 c2)))

; DB DB -> DB
; produce a new database with the same schema as db1 and db2 and the joint content of both
(check-expect (db-content (db-union DB1 DB2)) C1+2)

(define (db-union db1 db2)
  (make-db (db-schema db1)
           (content-union (db-content db1)
                          (db-content db2))))
