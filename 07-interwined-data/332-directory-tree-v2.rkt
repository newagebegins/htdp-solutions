;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 332-directory-tree-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct dir [name content])

; A Dir.v2 is a structure:
;   (make-dir String LOFD)

; An LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String.

(define CODE-DIR (make-dir "Code" (list "hang" "draw")))
(define DOCS-DIR (make-dir "Docs" (list "read!")))
(define LIBS-DIR (make-dir "Libs" (list CODE-DIR DOCS-DIR)))
(define TEXT-DIR (make-dir "Text" (list "part1" "part2" "part3")))
(define TS-DIR (make-dir "TS" (list TEXT-DIR "read!" LIBS-DIR)))
