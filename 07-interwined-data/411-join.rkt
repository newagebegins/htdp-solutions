;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 411-join) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A Group is [List X [List-of Row]]
; (X is the type of the first cell in a row)

(define S1 `(("Name" ,string?)
             ("Age" ,integer?)
             ("Present" ,boolean?)))
(define C1 '(("Alice" 35 #true)
             ("Bob" 25 #false)
             ("Carol" 30 #true)
             ("Dave" 32 #false)))
(define DB1 (make-db S1 C1))

(define S2 `(("Present" ,boolean?)
             ("Description" ,string?)))
(define C2 '((#true "presence")
             (#false "absence")))
(define DB2 (make-db S2 C2))

(define S1+2 `(("Name" ,string?)
               ("Age" ,integer?)
               ("Description" ,string?)))
(define C1+2 '(("Alice" 35 "presence")
               ("Bob" 25 "absence")
               ("Carol" 30 "presence")
               ("Dave" 32 "absence")))

(define S3 S2)
(define C3 '((#true "presence")
             (#true "here")
             (#false "absence")
             (#false "there")))
(define DB3 (make-db S3 C3))

(define S1+3 S1+2)
(define C1+3 '(("Alice" 35 "presence")
               ("Alice" 35 "here")
               ("Bob" 25 "absence")
               ("Bob" 25 "there")
               ("Carol" 30 "presence")
               ("Carol" 30 "here")
               ("Dave" 32 "absence")
               ("Dave" 32 "there")))

(define S4 `(("Present" ,boolean?)
             ("Description" ,string?)
             ("Points" ,integer?)))
(define C4 '((#true "presence" 1)
             (#false "absence" 2)
             (#true "hello" 55)))
(define DB4 (make-db S4 C4))

(define S1+4 `(("Name" ,string?)
               ("Age" ,integer?)
               ("Description" ,string?)
               ("Points" ,integer?)))
(define C1+4 '(("Alice" 35 "presence" 1)
               ("Alice" 35 "hello" 55)
               ("Bob" 25 "absence" 2)
               ("Carol" 30 "presence" 1)
               ("Carol" 30 "hello" 55)
               ("Dave" 32 "absence" 2)))

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

; List -> List
; remove the last item on the list
(check-expect (remove-last '()) '())
(check-expect (remove-last '(a)) '())
(check-expect (remove-last '(a b)) '(a))
(check-expect (remove-last '(a b c)) '(a b))

(define (remove-last l)
  (cond
    [(<= (length l) 1) '()]
    [else (cons (first l) (remove-last (rest l)))]))

; Schema Schema -> Schema
(check-expect (map first (join-schema S1 S2)) (map first S1+2))
(check-expect (map first (join-schema S1 S3)) (map first S1+3))
(check-expect (map first (join-schema S1 S4)) (map first S1+4))

(define (join-schema s1 s2)
  (append (remove-last s1) (rest s2)))

; Any Content -> Content
; collect all the rows with x in the first cell
(check-expect (rows-starting-with 'a '((a 1 2) (b 3 4) (a 5 6) (b 7 8) (b 9 0) (c 10 11)))
              '((1 2) (5 6)))
(check-expect (rows-starting-with 'b '((a 1 2) (b 3 4) (a 5 6) (b 7 8) (b 9 0) (c 10 11)))
              '((3 4) (7 8) (9 0)))
(check-expect (rows-starting-with 'c '((a 1 2) (b 3 4) (a 5 6) (b 7 8) (b 9 0) (c 10 11)))
              '((10 11)))

(define (rows-starting-with x c)
  (foldr
   ; Row Content -> Content
   (lambda (r res)
     (if (equal? (first r) x)
         (cons (rest r) res)
         res))
   '() c))

; Content -> [List-of Group]
; group rows by the value in the first cell
(check-expect (group-rows '((a 1 2) (b 3 4) (a 5 6) (b 7 8) (b 9 0) (c 10 11)))
              '((a ((1 2) (5 6))) (b ((3 4) (7 8) (9 0))) (c ((10 11)))))

(define (group-rows c)
  (map (lambda (x) (list x (rows-starting-with x c)))
       (remove-duplicates (map first c))))

; [List-of X] -> X
; produce the last item in the list
(check-expect (last '(1 2 3)) 3)
(check-error (last '()))

(define (last l)
  (first (reverse l)))

; Row Content -> Content
; join left table row with one or more right table rows
(check-expect (join-rows '(a b) '((1 2) (3 4)))
              '((a b 1 2) (a b 3 4)))

(define (join-rows lr rc)
  (map
   ; Row -> Row
   (lambda (r) (append lr r))
   rc))

; Content Content -> Content
(check-expect (join-content C1 C2) C1+2)
(check-expect (join-content C1 C3) C1+3)
(check-expect (join-content C1 C4) C1+4)

(define (join-content c1 c2)
  (local (; [List-of Group]
          (define groups (group-rows c2)))
    (foldr
     ; Row Content -> Content
     (lambda (r res)
       (append (join-rows (remove-last r)
                          (second (assoc (last r) groups)))
               res))
     '()
     c1)))

; DB DB -> DB
; create a database from db1 by replacing the last cell in each row
; with the translation of the cell in db2
(check-expect (map first (db-schema (join DB1 DB2))) (map first S1+2))
(check-expect (db-content (join DB1 DB2)) C1+2)

(check-expect (map first (db-schema (join DB1 DB3))) (map first S1+3))
(check-expect (db-content (join DB1 DB3)) C1+3)

(check-expect (map first (db-schema (join DB1 DB4))) (map first S1+4))
(check-expect (db-content (join DB1 DB4)) C1+4)

(define (join db1 db2)
  (make-db (join-schema (db-schema db1) (db-schema db2))
           (join-content (db-content db1) (db-content db2))))
