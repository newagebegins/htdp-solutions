;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |190|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-1string is one of:
; - '()
; - (cons 1String List-of-1string)

; A List-of-list-of-1string is one of:
; - '()
; - (cons List-of-1string List-of-list-of-1string)

; List-of-list-of-1string -> List-of-list-of-1string
; prepend s to all the lists in ll
(check-expect (prepend "a" '()) '())
(check-expect (prepend "a" (list (list "b") (list "b" "c")))
              (list (list "a" "b") (list "a" "b" "c")))

(define (prepend s ll)
  (cond
    [(empty? ll) '()]
    [else (cons (cons s (first ll)) (prepend s (rest ll)))]))

; List-of-1string -> List-of-list-of-1string
; produce all the prefixes of l
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b")) (list (list "a") (list "a" "b")))
(check-expect (prefixes (list "a" "b" "c")) (list (list "a") (list "a" "b") (list "a" "b" "c")))

(define (prefixes l)
  (cond
    [(empty? l) '()]
    [else (cons (list (first l)) (prepend (first l) (prefixes (rest l))))]))

; List-of-1string -> List-of-list-of-1string
; produce all the prefixes of l
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a")) (list (list "a")))
(check-expect (suffixes (list "a" "b")) (list (list "b") (list "a" "b")))
(check-expect (suffixes (list "a" "b" "c")) (list (list "c") (list "b" "c") (list "a" "b" "c")))

(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else (append (suffixes (rest l)) (list l))]))
