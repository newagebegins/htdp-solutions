;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 343-ls-r) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; A Path is [List-of String].
; interpretation: directions into a directory tree

(define F1 (make-file "F1" 1 ""))
(define F2 (make-file "F2" 2 ""))
(define F3 (make-file "F3" 3 ""))
(define F4 (make-file "F4" 4 ""))

; String [List-of Path] -> [List-of Path]
; prepend a directory name to the paths in the given list
(check-expect (prepend-dir-name "A1" '()) '())
(check-expect (prepend-dir-name "A1" '(("F1") ("F2") ("A1-1" "F3")))
              '(("A1" "F1") ("A1" "F2") ("A1" "A1-1" "F3")))

(define (prepend-dir-name name paths)
  (map (lambda (p) (cons name p)) paths))

; [List-of File] -> [List-of Path]
(check-expect (ls-files '()) '())
(check-expect (ls-files (list F1 F2)) '(("F1") ("F2")))

(define (ls-files files)
  (map (lambda (f) (list (file-name f))) files))

; [List-of Dir] -> [List-of Path]
(check-expect (ls-dirs '()) '())
(check-expect (ls-dirs (list (make-dir "A1" '() '()))) '())
(check-expect (ls-dirs (list (make-dir "A1-1" '() (list F1 F2 F3))
                             (make-dir "A1-2"
                                       (list (make-dir "A1-2-1" '() (list F3)))
                                       (list F1 F4))
                             (make-dir "A1-3" '() '())))
              '(("A1-1" "F1")
                ("A1-1" "F2")
                ("A1-1" "F3")
                ("A1-2" "F1")
                ("A1-2" "F4")
                ("A1-2" "A1-2-1" "F3")))

(define (ls-dirs dirs)
  (foldr (lambda (d lop) (append (ls-R d) lop)) '() dirs))

; Dir -> [List-of Path]
; list the paths to all files contained in a given Dir
(check-expect (ls-R (make-dir "A1" '() '()))
              '())
(check-expect (ls-R (make-dir "A1"
                              (list (make-dir "A1-1" '() '()))
                              '()))
              '())
(check-expect (ls-R (make-dir "A1" '() (list F1 F2)))
              '(("A1" "F1") ("A1" "F2")))
(check-expect (ls-R (make-dir "A1"
                              (list (make-dir "A1-1" '() (list F1 F2 F3))
                                    (make-dir "A1-2"
                                              (list (make-dir "A1-2-1" '() (list F3)))
                                              (list F1 F4))
                                    (make-dir "A1-3" '() '()))
                              (list F4)))
              '(("A1" "F4")
                ("A1" "A1-1" "F1") ("A1" "A1-1" "F2") ("A1" "A1-1" "F3")
                ("A1" "A1-2" "F1") ("A1" "A1-2" "F4")
                ("A1" "A1-2" "A1-2-1" "F3")))

(define (ls-R dir)
  (prepend-dir-name (dir-name dir)
                    (append (ls-files (dir-files dir))
                            (ls-dirs (dir-dirs dir)))))
