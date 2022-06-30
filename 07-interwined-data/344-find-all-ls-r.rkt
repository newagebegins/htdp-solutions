;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 344-find-all-ls-r) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; A Path is [List-of String].
; interpretation: directions into a directory tree

(define F1 (make-file "F1" 1 ""))
(define F2 (make-file "F2" 2 ""))
(define F3 (make-file "F3" 3 ""))
(define F4 (make-file "F4" 4 ""))

; String [List-of Path] -> [List-of Path]
; prepend a directory name to the paths in the given list
(define (prepend-dir-name name paths)
  (map (lambda (p) (cons name p)) paths))

; [List-of File] -> [List-of Path]
(define (ls-files files)
  (map (lambda (f) (list (file-name f))) files))

; [List-of Dir] -> [List-of Path]
(define (ls-dirs dirs)
  (foldr (lambda (d lop) (append (ls-R d) lop)) '() dirs))

; Dir -> [List-of Path]
; list the paths to all files contained in a given Dir
(define (ls-R dir)
  (prepend-dir-name (dir-name dir)
                    (append (ls-files (dir-files dir))
                            (ls-dirs (dir-dirs dir)))))

; [X] [Non-empty-list-of X] -> X
; return the last item in a list
(check-expect (last '("foo")) "foo")
(check-expect (last '("foo" "bar" "baz")) "baz")

(define (last l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (last (rest l))]))

; Dir String -> [List-of Path]
; returns a list of paths to the files named fn in the directory tree dir
(check-expect (find-all (make-dir "A1" '() '()) "F1") '())
(check-expect (find-all (make-dir "A1" '() (list F1 F2 F3)) "F2") '(("A1" "F2")))
(check-expect (find-all (make-dir "A1"
                                  (list (make-dir "A1-1"
                                                  (list (make-dir "A1-1-1" '() (list F1 F2))
                                                        (make-dir "A1-1-2"
                                                                  (list (make-dir "A1-1-2-1" '() (list F2 F3)))
                                                                  (list F1 F2 F3))
                                                        (make-dir "A1-1-3" '() (list F4)))
                                                  (list F1 F2 F4))
                                        (make-dir "A1-2" '() (list F2))
                                        (make-dir "A1-3" '() '()))
                                  (list F1 F2 F3 F4))
                        "F2")
              '(("A1" "F2")
                ("A1" "A1-1" "F2")
                ("A1" "A1-1" "A1-1-1" "F2")
                ("A1" "A1-1" "A1-1-2" "F2")
                ("A1" "A1-1" "A1-1-2" "A1-1-2-1" "F2")
                ("A1" "A1-2" "F2")))

(define (find-all dir fn)
  (filter (lambda (p)
            (string=? (last p) fn))
          (ls-R dir)))
