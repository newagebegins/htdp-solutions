;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |274|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1String] -> [List-of [List-of 1String]]
; produce all the prefixes of l
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b")) (list (list "a") (list "a" "b")))
(check-expect (prefixes (list "a" "b" "c")) (list (list "a") (list "a" "b") (list "a" "b" "c")))

(define (prefixes l)
  (local (; 1String [List-of [List-of 1String]] -> [List-of [List-of 1String]]
          (define (f x y)
            (cond
              [(empty? y) (list (list x))]
              [else (cons (cons x (first y)) y)])))
    (map reverse (reverse (foldl f '() l)))))

; [List-of 1String] -> [List-of [List-of 1String]]
; produce all the prefixes of l
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a")) (list (list "a")))
(check-expect (suffixes (list "a" "b")) (list (list "b") (list "a" "b")))
(check-expect (suffixes (list "a" "b" "c")) (list (list "c") (list "b" "c") (list "a" "b" "c")))

(define (suffixes l)
  (local (; 1String [List-of [List-of 1String]] -> [List-of [List-of 1String]]
          (define (f x y)
            (cond
              [(empty? y) (list (list x))]
              [else (cons (cons x (first y)) y)])))
    (reverse (foldr f '() l))))
