;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 336-how-many-v3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct file [name size content])

; A File.v3 is a structure:
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure:
;   (make-dir.v3 String Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

(define HANG-FILE (make-file "hang" 8 ""))
(define DRAW-FILE (make-file "draw" 2 ""))
(define READ1-FILE (make-file "read!" 19 ""))
(define READ2-FILE (make-file "read!" 10 ""))
(define PART1-FILE (make-file "part1" 99 ""))
(define PART2-FILE (make-file "part2" 52 ""))
(define PART3-FILE (make-file "part3" 17 ""))

(define CODE-DIR (make-dir.v3 "Code" '() (list HANG-FILE DRAW-FILE)))
(define DOCS-DIR (make-dir.v3 "Docs" '() (list READ1-FILE)))
(define LIBS-DIR (make-dir.v3 "Libs" (list CODE-DIR DOCS-DIR) '()))
(define TEXT-DIR (make-dir.v3 "Text" '() (list PART1-FILE PART2-FILE PART3-FILE)))
(define TS-DIR (make-dir.v3 "TS" (list TEXT-DIR LIBS-DIR) (list READ2-FILE)))

; Dir.v3 -> N
; returns the number of files in the given directory (recursively)
(check-expect (how-many  (make-dir.v3 "hello" '() '())) 0)
(check-expect (how-many CODE-DIR) 2)
(check-expect (how-many DOCS-DIR) 1)
(check-expect (how-many TEXT-DIR) 3)
(check-expect (how-many LIBS-DIR) 3)
(check-expect (how-many TS-DIR) 7)

(define (how-many d)
  (local (; Dir* -> N
          (define (how-many-dir* d*)
            (cond
              [(empty? d*) 0]
              [else (+ (how-many (first d*))
                       (how-many-dir* (rest d*)))]))
          ; File* -> N
          (define (how-many-file* f*)
            (length f*)))
    (+ (how-many-dir* (dir.v3-dirs d))
       (how-many-file* (dir.v3-files d)))))
