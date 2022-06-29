;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 342-find) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; A Path is [List-of String].
; interpretation: directions into a directory tree

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

; Dir String -> [Maybe Path]
; return the path to a file with the name fn or #false if the file was not found
(check-expect (find CODE-DIR "hang") '("Code" "hang"))
(check-expect (find CODE-DIR "foo") #false)
(check-expect (find TS-DIR "read!") '("TS" "read!"))
(check-expect (find TS-DIR "part2") '("TS" "Text" "part2"))
(check-expect (find TS-DIR "part4") #false)
(check-expect (find TS-DIR "draw") '("TS" "Libs" "Code" "draw"))

(define (find d fn)
  (local (; [List-of File] -> Boolean
          (define (find-in-files? files)
            (ormap (lambda (x)
                     (string=? (file-name x) fn))
                   files)))
    (cond
      [(find-in-files? (dir-files d)) (list (dir-name d) fn)]
      [else (local (; [List-of Dir] -> [Maybe Path]
                    (define (find-in-dirs dirs)
                      (cond
                        [(empty? dirs) #false]
                        [else (local ((define found (find (first dirs) fn)))
                                (if (false? found)
                                    (find-in-dirs (rest dirs))
                                    found))]))
                    ; [Maybe Path]
                    (define maybe-path (find-in-dirs (dir-dirs d))))
              (if (false? maybe-path)
                  #false
                  (cons (dir-name d) maybe-path)))])))
