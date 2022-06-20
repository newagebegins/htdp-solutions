;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |213|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1String List-of-words -> List-of-words
; prepend the letter l to all words in low
(check-expect (prepend-letter "a" '())
              '())
(check-expect (prepend-letter "a" (list (list "b")))
              (list (list "a" "b")))
(check-expect (prepend-letter "e" (list (list "d" "r") (list "r" "d")))
              (list (list "e" "d" "r") (list "e" "r" "d")))

(define (prepend-letter l low)
  (cond
    [(empty? low) '()]
    [else (cons (cons l (first low)) (prepend-letter l (rest low)))]))

; 1String Word -> List-of-words
; produce a list of words with l inserted at the beginning, between all letters and at the end of the word w
(check-expect (insert-everywhere "f" '())
              (list (list "f")))
(check-expect (insert-everywhere "a" (list "b"))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere "d" (list "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))

(define (insert-everywhere l w)
  (cond
    [(empty? w) (list (list l))]
    [else (cons (cons l w)
                (prepend-letter (first w) (insert-everywhere l (rest w))))]))

; 1String List-of-words -> List-of-words
; produces a list like low, but with l inserted at the beginning, between all letters,
; and at the end of all words in the given list
(check-expect (insert-everywhere/in-all-words "a" '())
              '())
(check-expect (insert-everywhere/in-all-words "a" (list '()))
              (list (list "a")))
(check-expect (insert-everywhere/in-all-words "b" (list (list "c")))
              (list (list "b" "c") (list "c" "b")))
(check-expect (insert-everywhere/in-all-words "d" (list (list "e" "r") (list "r" "e")))
              (list
               (list "d" "e" "r")
               (list "e" "d" "r")
               (list "e" "r" "d")
               (list "d" "r" "e")
               (list "r" "d" "e")
               (list "r" "e" "d")))

(define (insert-everywhere/in-all-words l low)
  (cond
    [(empty? low) '()]
    [else (append (insert-everywhere l (first low)) (insert-everywhere/in-all-words l (rest low)))]))
