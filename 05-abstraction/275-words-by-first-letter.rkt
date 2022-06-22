;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 275-words-by-first-letter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
  (foldr insert-word '() d))
