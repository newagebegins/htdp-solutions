;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 339-find) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

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

; Dir String -> Boolean
; return #true if a file with the name fn is found in the directory tree d0
(check-expect (find? CODE-DIR "hang") #true)
(check-expect (find? CODE-DIR "foo") #false)
(check-expect (find? TS-DIR "read!") #true)
(check-expect (find? TS-DIR "part2") #true)
(check-expect (find? TS-DIR "part4") #false)

(define (find? d0 fn)
  (or (ormap (lambda (d) (find? d fn)) (dir-dirs d0))
      (ormap (lambda (f) (string=? (file-name f) fn)) (dir-files d0))))
