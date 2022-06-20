;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |198|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; On OS X:
(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

(define-struct letter-count [letter count])
; A LetterCount is a structure:
;   (make-letter-count Letter Number)

; A List-of-letter-counts is one of:
; - '()
; - (cons LetterCount List-of-letter-counts)

; Letter LetterCount -> LetterCount
; if l is the same letter as lc increment the count of lc by one
(check-expect (increment-letter-count "x" (make-letter-count "x" 3)) (make-letter-count "x" 4))
(check-expect (increment-letter-count "c" (make-letter-count "x" 3)) (make-letter-count "x" 3))
(check-expect (increment-letter-count "b" (make-letter-count "a" 0)) (make-letter-count "a" 0))

(define (increment-letter-count l lc)
  (make-letter-count (letter-count-letter lc)
                     (+ (letter-count-count lc)
                        (if (string=? (letter-count-letter lc) l)
                            1
                            0))))

; Letter List-of-letter-counts -> List-of-letter-counts
; increment by one the count of the given letter in the list of letter counts
(check-expect (letter+1 "a" '()) '())
(check-expect (letter+1 "a" (list (make-letter-count "a" 0))) (list (make-letter-count "a" 1)))
(check-expect (letter+1 "a" (list (make-letter-count "a" 1))) (list (make-letter-count "a" 2)))
(check-expect (letter+1 "b" (list (make-letter-count "b" 0))) (list (make-letter-count "b" 1)))
(check-expect (letter+1 "b" (list (make-letter-count "a" 1)
                                  (make-letter-count "b" 2)
                                  (make-letter-count "c" 3)))
              (list (make-letter-count "a" 1)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 3)))

(define (letter+1 let llc)
  (cond
    [(empty? llc) '()]
    [else (cons (increment-letter-count let (first llc))
                (letter+1 let (rest llc)))]))

; Dictionary -> List-of-letter-counts
; count how often each letter is used as the first one of a word in the given dictionary
(check-expect (count-by-letter '())
              (list (make-letter-count "a" 0)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 0)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 0)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 0)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 0)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 0)))

(check-expect (count-by-letter (list "Amiga" "apple" "commodore"  "sega" "sony" "spectrum"))
              (list (make-letter-count "a" 2)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 1)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 0)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 0)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 3)
                    (make-letter-count "t" 0)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 0)))

(define (count-by-letter d)
  (cond
    [(empty? d) (list (make-letter-count "a" 0)
                      (make-letter-count "b" 0)
                      (make-letter-count "c" 0)
                      (make-letter-count "d" 0)
                      (make-letter-count "e" 0)
                      (make-letter-count "f" 0)
                      (make-letter-count "g" 0)
                      (make-letter-count "h" 0)
                      (make-letter-count "i" 0)
                      (make-letter-count "j" 0)
                      (make-letter-count "k" 0)
                      (make-letter-count "l" 0)
                      (make-letter-count "m" 0)
                      (make-letter-count "n" 0)
                      (make-letter-count "o" 0)
                      (make-letter-count "p" 0)
                      (make-letter-count "q" 0)
                      (make-letter-count "r" 0)
                      (make-letter-count "s" 0)
                      (make-letter-count "t" 0)
                      (make-letter-count "u" 0)
                      (make-letter-count "v" 0)
                      (make-letter-count "w" 0)
                      (make-letter-count "x" 0)
                      (make-letter-count "y" 0)
                      (make-letter-count "z" 0))]
    [else (letter+1 (string-downcase (string-ith (first d) 0))
                    (count-by-letter (rest d)))]))

; LetterCount LetterCount -> LetterCount
; given two LetterCounts select the one with the greatest count
(check-expect (select-max-count (make-letter-count "a" 0) (make-letter-count "b" 1)) (make-letter-count "b" 1))
(check-expect (select-max-count (make-letter-count "a" 2) (make-letter-count "b" 1)) (make-letter-count "a" 2))
(check-expect (select-max-count (make-letter-count "a" 3) (make-letter-count "b" 3)) (make-letter-count "a" 3))

(define (select-max-count lc1 lc2)
  (if (>= (letter-count-count lc1) (letter-count-count lc2))
      lc1
      lc2))

; List-of-letter-counts -> LetterCount
; from the given list select the LetterCount with the greatest count
(check-expect (max-count '())
              (make-letter-count "a" 0))
(check-expect (max-count (list (make-letter-count "b" 2)))
              (make-letter-count "b" 2))
(check-expect (max-count (list (make-letter-count "a" 1)
                               (make-letter-count "b" 3)
                               (make-letter-count "c" 0)))
              (make-letter-count "b" 3))

(define (max-count llc)
  (cond
    [(empty? llc) (make-letter-count "a" 0)]
    [else (select-max-count (first llc) (max-count (rest llc)))]))

; Dictionary -> LetterCount
; produce the LetterCount for the letter that occurs most often in the given Dictionary
(check-expect (most-frequent '()) (make-letter-count "a" 0))
(check-expect (most-frequent (list "apple")) (make-letter-count "a" 1))
(check-expect (most-frequent (list "apple" "banana")) (make-letter-count "a" 1))
(check-expect (most-frequent (list "banana" "blueberry")) (make-letter-count "b" 2))
(check-expect (most-frequent (list "apple" "banana" "pear" "pomegranate" "pineapple" "raspberry"))
              (make-letter-count "p" 3))
(check-expect (most-frequent (list "Apple")) (make-letter-count "a" 1))

(define (most-frequent d)
  (max-count (count-by-letter d)))

; A List-of-dictionaries is one of:
; - '()
; - (cons Dictionary List-of-dictionaries)

; String Dictionary -> Boolean
; return #true when dictionary d contains words that start with the same letter as the word w
(check-expect (can-append? "app" (list "ape")) #true)
(check-expect (can-append? "app" (list "banana")) #false)

(check-expect (can-append? "App" (list "ape")) #true)
(check-expect (can-append? "app" (list "Ape")) #true)

(define (can-append? w d)
  (string-ci=? (string-ith w 0) (string-ith (first d) 0)))

; String List-of-dictionaries -> List-of-dictionaries
; insert the given word into the dictionary for the corresponding letter in the list of dictionaries              
(check-expect (insert-word "ape" '()) (list (list "ape")))
                                       
(check-expect (insert-word "ape" (list (list "app" "apple")
                                       (list "banana")
                                       (list "dog" "doom" "dull")
                                       (list "zoo")))
              (list (list "ape" "app" "apple")
                    (list "banana")
                    (list "dog" "doom" "dull")
                    (list "zoo")))

(check-expect (insert-word "ape" (list (list "banana")
                                       (list "dog" "doom" "dull")
                                       (list "zoo")))
              (list (list "ape")
                    (list "banana")
                    (list "dog" "doom" "dull")
                    (list "zoo")))

(define (insert-word w lod)
  (cond
    [(empty? lod) (list (list w))]
    [else (if (can-append? w (first lod))
              (cons (append (list w) (first lod)) (rest lod))
              (cons (list w) lod))]))

; Dictionary -> List-of-dictionaries
; from the given dictionary produce a list of dictionaries, one per letter
(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter (list "apple")) (list (list "apple")))
(check-expect (words-by-first-letter (list "ape" "apple" "banana"))
              (list (list "ape" "apple")
                    (list "banana")))
(check-expect (words-by-first-letter (list "ape" "apple" "banana" "dog" "doom" "dull" "zoo"))
              (list (list "ape" "apple")
                    (list "banana")
                    (list "dog" "doom" "dull")
                    (list "zoo")))
(check-expect (words-by-first-letter (list "ape" "banana" "dog" "doom" "dull" "zoo"))
              (list (list "ape")
                    (list "banana")
                    (list "dog" "doom" "dull")
                    (list "zoo")))
(check-expect (words-by-first-letter (list "ape" "Apple" "Banana" "bear"))
              (list (list "ape" "Apple")
                    (list "Banana" "bear")))

(define (words-by-first-letter d)
  (cond
    [(empty? d) '()]
    [else (insert-word (first d) (words-by-first-letter (rest d)))]))

; Dictionary -> LetterCount
(check-expect (dict->lc (list "a1" "a2")) (make-letter-count "a" 2))
(check-expect (dict->lc (list "b1")) (make-letter-count "b" 1))
(check-expect (dict->lc (list "S1")) (make-letter-count "s" 1))

(define (dict->lc d)
  (make-letter-count (string-downcase (string-ith (first d) 0))
                     (length d)))

; LetterCount LetterCount -> LetterCount
(check-expect (max-lc (make-letter-count "a" 1) (make-letter-count "b" 2)) (make-letter-count "b" 2))
(check-expect (max-lc (make-letter-count "a" 2) (make-letter-count "b" 1)) (make-letter-count "a" 2))
(check-expect (max-lc (make-letter-count "a" 2) (make-letter-count "b" 2)) (make-letter-count "a" 2))

(define (max-lc lc1 lc2)
  (if (>= (letter-count-count lc1) (letter-count-count lc2))
      lc1
      lc2))

; List-of-dictionaries -> LetterCount
; find the biggest dictionary in the list
(check-expect (max-count.v2 '()) (make-letter-count "a" 0))
(check-expect (max-count.v2 (list (list "a1" "a2")
                                  (list "b1" "b2" "b3")
                                  (list "c")))
              (make-letter-count "b" 3))
(check-expect (max-count.v2 (list (list "a1" "a2")
                                  (list "b1")
                                  (list "c")))
              (make-letter-count "a" 2))

(define (max-count.v2 ld)
  (cond
    [(empty? ld) (make-letter-count "a" 0)]
    [else (max-lc (dict->lc (first ld)) (max-count.v2 (rest ld)))]))

;ld = (list (list "a1" "a2")
;           (list "b1" "b2" "b3")
;           (list "c"))
;(first ld) = (list "a1" "a2")
;(rest ld) = (list (list "b1" "b2" "b3")
;                  (list "c"))
;(max-count.v2 (rest ld)) = (make-letter-count "b" 3)
;(max-count.v2 ld) = (make-letter-count "b" 3)
;
;ld = (list (list "a1" "a2" "a3")
;           (list "b")
;           (list "c1" "c2"))
;(first ld) = (list "a1" "a2" "a3")
;(rest ld) = (list (list "b")
;                  (list "c1" "c2"))
;(max-count.v2 (rest ld)) = (make-letter-count "c" 2)
;(max-count.v2 ld) = (make-letter-count "a" 3)

; Dictionary -> LetterCount
; produce the LetterCount for the letter that occurs most often in the given Dictionary
(check-expect (most-frequent.v2 '()) (make-letter-count "a" 0))
(check-expect (most-frequent.v2 (list "apple")) (make-letter-count "a" 1))
(check-expect (most-frequent.v2 (list "apple" "banana")) (make-letter-count "a" 1))
(check-expect (most-frequent.v2 (list "banana" "blueberry")) (make-letter-count "b" 2))
(check-expect (most-frequent.v2 (list "apple" "banana" "pear" "pomegranate" "pineapple" "raspberry"))
              (make-letter-count "p" 3))
(check-expect (most-frequent.v2 (list "apple" "banana" "pear" "pomegranate" "pineapple" "raspberry" "u1" "u2" "u3" "u4"))
              (make-letter-count "u" 4))
(check-expect (most-frequent.v2 (list "a1" "a2" "a3" "a4" "b1" "b2" "b3" "b4" "b5" "c1" "c2" "c3"))
              (make-letter-count "b" 5))

(define (most-frequent.v2 d)
  (max-count.v2 (words-by-first-letter d)))

(check-expect (most-frequent AS-LIST)
              (most-frequent.v2 AS-LIST))
