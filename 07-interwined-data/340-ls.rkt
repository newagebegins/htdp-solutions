;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 340-ls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dir -> [List-of String]
; lists the names of all files and sub-directories in the given directory
(check-expect (ls CODE-DIR) '("hang" "draw"))
(check-expect (ls DOCS-DIR) '("read!"))
(check-expect (ls LIBS-DIR) '("Code" "Docs"))
(check-expect (ls TEXT-DIR) '("part1" "part2" "part3"))
(check-expect (ls TS-DIR) '("Text" "Libs" "read!"))

(define (ls d)
  (append (map dir-name (dir-dirs d))
          (map file-name (dir-files d))))
