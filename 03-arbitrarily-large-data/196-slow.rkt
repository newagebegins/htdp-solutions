;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 196.v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Letter Dictionary -> Number
; count how many words in the given dictionary start with the given letter
(check-expect (starts-with# "a" '()) 0)
(check-expect (starts-with# "a" (list "abu")) 1)
(check-expect (starts-with# "a" (list "bob")) 0)
(check-expect (starts-with# "a" (list "fun" "ace")) 1)
(check-expect (starts-with# "a" (list "ace" "fun")) 1)
(check-expect (starts-with# "a" (list "ace" "bob" "abu")) 2)
(check-expect (starts-with# "d" (list "fun" "day" "dog" "doom")) 3)

(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else (+ (if (string=? (string-ith (first d) 0) l)
                 1
                 0)
             (starts-with# l (rest d)))]))

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

(check-expect (count-by-letter (list "amiga" "apple" "commodore"  "sega" "sony" "spectrum"))
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
  (list (make-letter-count "a" (starts-with# "a" d))
        (make-letter-count "b" (starts-with# "b" d))
        (make-letter-count "c" (starts-with# "c" d))
        (make-letter-count "d" (starts-with# "d" d))
        (make-letter-count "e" (starts-with# "e" d))
        (make-letter-count "f" (starts-with# "f" d))
        (make-letter-count "g" (starts-with# "g" d))
        (make-letter-count "h" (starts-with# "h" d))
        (make-letter-count "i" (starts-with# "i" d))
        (make-letter-count "j" (starts-with# "j" d))
        (make-letter-count "k" (starts-with# "k" d))
        (make-letter-count "l" (starts-with# "l" d))
        (make-letter-count "m" (starts-with# "m" d))
        (make-letter-count "n" (starts-with# "n" d))
        (make-letter-count "o" (starts-with# "o" d))
        (make-letter-count "p" (starts-with# "p" d))
        (make-letter-count "q" (starts-with# "q" d))
        (make-letter-count "r" (starts-with# "r" d))
        (make-letter-count "s" (starts-with# "s" d))
        (make-letter-count "t" (starts-with# "t" d))
        (make-letter-count "u" (starts-with# "u" d))
        (make-letter-count "v" (starts-with# "v" d))
        (make-letter-count "w" (starts-with# "w" d))
        (make-letter-count "x" (starts-with# "x" d))
        (make-letter-count "y" (starts-with# "y" d))
        (make-letter-count "z" (starts-with# "z" d))))
