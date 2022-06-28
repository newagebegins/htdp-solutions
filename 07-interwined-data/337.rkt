;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |337|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct file [name size content])

; A File is a structure:
;   (make-file String N String)

(define-struct dir [name dirs files])

; A Dir is a structure:
;   (make-dir String [List-of Dir] [List-of File])

(define HANG-FILE (make-file "hang" 8 ""))
(define DRAW-FILE (make-file "draw" 2 ""))
(define READ1-FILE (make-file "read!" 19 ""))
(define READ2-FILE (make-file "read!" 10 ""))
(define PART1-FILE (make-file "part1" 99 ""))
(define PART2-FILE (make-file "part2" 52 ""))
(define PART3-FILE (make-file "part3" 17 ""))

(define CODE-DIR (make-dir "Code" '() (list HANG-FILE DRAW-FILE)))
(define DOCS-DIR (make-dir "Docs" '() (list READ1-FILE)))
(define LIBS-DIR (make-dir "Libs" (list CODE-DIR DOCS-DIR) '()))
(define TEXT-DIR (make-dir "Text" '() (list PART1-FILE PART2-FILE PART3-FILE)))
(define TS-DIR (make-dir "TS" (list TEXT-DIR LIBS-DIR) (list READ2-FILE)))

; Dir -> N
; returns the number of files in the given directory
(check-expect (how-many CODE-DIR) 2)
(check-expect (how-many DOCS-DIR) 1)
(check-expect (how-many TEXT-DIR) 3)
(check-expect (how-many LIBS-DIR) 0)
(check-expect (how-many TS-DIR) 1)

(define (how-many d)
  (length (dir-files d)))
