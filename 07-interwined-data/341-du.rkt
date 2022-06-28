;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 341-du) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

(define HANG-FILE (make-file "hang" 8 ""))
(define DRAW-FILE (make-file "draw" 2 ""))
(define READ1-FILE (make-file "read!" 19 ""))
(define READ2-FILE (make-file "read!" 10 ""))
(define PART1-FILE (make-file "part1" 99 ""))
(define PART2-FILE (make-file "part2" 52 ""))
(define PART3-FILE (make-file "part3" 17 ""))

(define CODE-DIR (make-dir "Code" '() (list HANG-FILE DRAW-FILE)))
(define CODE-DIR-SIZE (+ 8 2 1))

(define DOCS-DIR (make-dir "Docs" '() (list READ1-FILE)))
(define DOCS-DIR-SIZE (+ 19 1))

(define LIBS-DIR (make-dir "Libs" (list CODE-DIR DOCS-DIR) '()))
(define LIBS-DIR-SIZE (+ CODE-DIR-SIZE DOCS-DIR-SIZE 1))

(define TEXT-DIR (make-dir "Text" '() (list PART1-FILE PART2-FILE PART3-FILE)))
(define TEXT-DIR-SIZE (+ 99 52 17 1))

(define TS-DIR (make-dir "TS" (list TEXT-DIR LIBS-DIR) (list READ2-FILE)))
(define TS-DIR-SIZE (+ TEXT-DIR-SIZE LIBS-DIR-SIZE 10 1))

; Dir -> N
; compute the total size of a directory tree
; (assuming that storing a directory in a Dir structure costs 1 file storage unit)
(check-expect (du CODE-DIR) CODE-DIR-SIZE)
(check-expect (du DOCS-DIR) DOCS-DIR-SIZE)
(check-expect (du LIBS-DIR) LIBS-DIR-SIZE)
(check-expect (du TEXT-DIR) TEXT-DIR-SIZE)
(check-expect (du TS-DIR) TS-DIR-SIZE)

(define (du d0)
  (+ 1
     (foldr (lambda (d s) (+ (du d) s)) 0 (dir-dirs d0))
     (foldr (lambda (f s) (+ (file-size f) s)) 0 (dir-files d0))))
