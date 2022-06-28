;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 331-how-many-files) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A File.v1 is a String.

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

(define CODE-DIR '("hang" "draw"))
(define DOCS-DIR '("read!"))
(define TEXT-DIR '("part1" "part2" "part3"))
(define LIBS-DIR (list CODE-DIR DOCS-DIR))
(define TS-DIR (list TEXT-DIR "read!" LIBS-DIR))

; Dir.v1 -> N
; returns the number of files in the given directory
(check-expect (how-many '()) 0)
(check-expect (how-many CODE-DIR) 2)
(check-expect (how-many DOCS-DIR) 1)
(check-expect (how-many TEXT-DIR) 3)
(check-expect (how-many LIBS-DIR) 0)
(check-expect (how-many TS-DIR) 1)

(define (how-many dir)
  (length (filter string? dir)))
