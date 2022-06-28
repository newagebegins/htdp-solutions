;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 334-dir-size-and-readability) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct dir [name size readability content])

; A Dir is a structure:
;   (make-dir String N Boolean LOFD)

; An LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File LOFD)
; - (cons Dir LOFD)

; A File is a String.

(define CODE-DIR (make-dir "Code" 1 #true (list "hang" "draw")))
(define DOCS-DIR (make-dir "Docs" 2 #false (list "read!")))
(define LIBS-DIR (make-dir "Libs" 1 #true (list CODE-DIR DOCS-DIR)))
(define TEXT-DIR (make-dir "Text" 3 #true (list "part1" "part2" "part3")))
(define TS-DIR (make-dir "TS" 1 #false (list TEXT-DIR "read!" LIBS-DIR)))
