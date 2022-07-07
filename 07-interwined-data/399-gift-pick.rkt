;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 399-gift-pick) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of X] -> X
; returns a random item from the list
(check-expect (random-pick '("foo")) "foo")
(check-member-of (random-pick '(a b c)) 'a 'b 'c)

(define (random-pick l)
  (list-ref l (random (length l))))

; [List-of String] [List-of String] -> Boolean
; returns #true if l1 and l2 do not agree at any place
(check-expect (non-same? '("a" "b" "c") '("b" "c" "a")) #true)
(check-expect (non-same? '("a" "b" "c") '("b" "a" "c")) #false)

(define (non-same? l1 l2)
  (cond
    [(empty? l1) #true]
    [else (and (not (string=? (first l1) (first l2)))
               (non-same? (rest l1) (rest l2)))]))

; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
; produces the list of those lists in ll that do not agree with names at any place
(check-expect (non-same '("a" "b") '(("a" "b")
                                     ("b" "a")))
              '(("b" "a")))
(check-expect (non-same '("a" "b" "c") '(("a" "b" "c")
                                         ("a" "c" "b")
                                         ("b" "a" "c")
                                         ("b" "c" "a")
                                         ("c" "a" "b")
                                         ("c" "b" "a")))
              '(("b" "c" "a")
                ("c" "a" "b")))

(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [else (if (non-same? names (first ll))
              (cons (first ll) (non-same names (rest ll)))
              (non-same names (rest ll)))]))

; [List-of String] -> [List-of String]
; picks a random non-identity arrangement of names
(check-member-of (gift-pick '("a" "b" "c")) '("b" "c" "a") '("c" "a" "b"))

(define (gift-pick names)
  (random-pick
   (non-same names (arrangements names))))

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(define (arrangements names)
  (local (; String [List-of [List-of String]] -> [List-of [List-of String]]
          (define (prepend l low)
            (cond
              [(empty? low) '()]
              [else (cons (cons l (first low)) (prepend l (rest low)))]))

          ; String [List-of String] -> [List-of [List-of String]]
          (define (insert-everywhere l w)
            (cond
              [(empty? w) (list (list l))]
              [else (cons (cons l w)
                          (prepend (first w) (insert-everywhere l (rest w))))]))

          ; String [List-of [List-of String]] -> [List-of [List-of String]]
          (define (insert-everywhere/in-all-words l low)
            (cond
              [(empty? low) '()]
              [else (append (insert-everywhere l (first low)) (insert-everywhere/in-all-words l (rest low)))])))
    (cond
      [(empty? names) (list '())]
      [else (insert-everywhere/in-all-words (first names)
                                            (arrangements (rest names)))])))
