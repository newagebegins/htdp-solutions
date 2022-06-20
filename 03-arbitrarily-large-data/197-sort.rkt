;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 197.sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
    [else (letter+1 (string-ith (first d) 0)
                    (count-by-letter (rest d)))]))

; LetterCount LetterCount -> Boolean
; return lc1 if the count of lc1 is less than the count of lc2
(check-expect (letter-count< (make-letter-count "a" 0) (make-letter-count "b" 1)) #true)
(check-expect (letter-count< (make-letter-count "a" 2) (make-letter-count "b" 1)) #false)
(check-expect (letter-count< (make-letter-count "a" 1) (make-letter-count "b" 1)) #false)

(define (letter-count< lc1 lc2)
  (< (letter-count-count lc1) (letter-count-count lc2)))

; LetterCount List-of-letter-counts -> List-of-letter-counts
; insert the given LetterCount into the sorted list and keep the list sorted (in descending order)
(check-expect (insert (make-letter-count "b" 1) '()) (list (make-letter-count "b" 1)))

(check-expect (insert (make-letter-count "b" 1)
                      (list (make-letter-count "a" 0)))
              (list (make-letter-count "b" 1)
                    (make-letter-count "a" 0)))

(check-expect (insert (make-letter-count "b" 0)
                      (list (make-letter-count "a" 1)))
              (list (make-letter-count "a" 1)
                    (make-letter-count "b" 0)))

(check-expect (insert (make-letter-count "a" 2)
                      (list (make-letter-count "b" 10)
                            (make-letter-count "d" 7)
                            (make-letter-count "c" 1)))
              (list
               (make-letter-count "b" 10)
               (make-letter-count "d" 7)
               (make-letter-count "a" 2)
               (make-letter-count "c" 1)))

(check-expect (insert (make-letter-count "a" 20)
                      (list (make-letter-count "b" 10)
                            (make-letter-count "d" 7)
                            (make-letter-count "c" 1)))
              (list
               (make-letter-count "a" 20)
               (make-letter-count "b" 10)
               (make-letter-count "d" 7)
               (make-letter-count "c" 1)))

(check-expect (insert (make-letter-count "a" 10)
                      (list (make-letter-count "b" 10)
                            (make-letter-count "d" 7)
                            (make-letter-count "c" 1)))
              (list
               (make-letter-count "b" 10)
               (make-letter-count "a" 10)
               (make-letter-count "d" 7)
               (make-letter-count "c" 1)))

(define (insert lc llc)
  (cond
    [(empty? llc) (list lc)]
    [else (if (letter-count< (first llc) lc)
              (cons lc llc)
              (cons (first llc) (insert lc (rest llc))))]))

; List-of-letter-counts -> List-of-letter-counts
; sort the letters by their counts in descending order
(check-expect (sort> '()) '())
(check-expect (sort> (list (make-letter-count "a" 2))) (list (make-letter-count "a" 2)))
(check-expect (sort> (list (make-letter-count "a" 2)
                           (make-letter-count "b" 10)
                           (make-letter-count "c" 1)
                           (make-letter-count "d" 7)))
              (list
               (make-letter-count "b" 10)
               (make-letter-count "d" 7)
               (make-letter-count "a" 2)
               (make-letter-count "c" 1)))

(define (sort> llc)
  (cond
    [(empty? llc) '()]
    [else (insert (first llc) (sort> (rest llc)))]))

; Dictionary -> LetterCount
; produce the LetterCount for the letter that occurs most often in the given Dictionary
(check-expect (most-frequent '()) (make-letter-count "z" 0))
(check-expect (most-frequent (list "apple")) (make-letter-count "a" 1))
(check-expect (most-frequent (list "apple" "banana")) (make-letter-count "b" 1))
(check-expect (most-frequent (list "banana" "blueberry")) (make-letter-count "b" 2))
(check-expect (most-frequent (list "apple" "banana" "pear" "pomegranate" "pineapple" "raspberry"))
              (make-letter-count "p" 3))

(define (most-frequent d)
  (first (sort> (count-by-letter d))))

; > (most-frequent AS-LIST)
; (make-letter-count "s" 22759)
