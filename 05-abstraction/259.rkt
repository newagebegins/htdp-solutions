;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |259|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interp. a Word is a list of 1Strings (letters)

; On OS X: 
(define LOCATION "/usr/share/dict/words")
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
   (member? "rat" w) (member? "art" w) (member? "tar" w)))

; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (local (
          ; String -> Word
          ; converts s to the chosen word representation
          (define (string->word s)
            (explode s))

          ; Word -> String
          ; converts w to a string
          (define (word->string w)
            (implode w))

          ; List-of-words -> List-of-strings
          ; turn all Words in low into Strings
          (define (words->strings low)
            (cond
              [(empty? low) '()]
              [else (cons (word->string (first low)) (words->strings (rest low)))]))
 
          ; 1String List-of-words -> List-of-words
          ; prepend the letter l to all words in low
          (define (prepend-letter l low)
            (cond
              [(empty? low) '()]
              [else (cons (cons l (first low)) (prepend-letter l (rest low)))]))

          ; 1String Word -> List-of-words
          ; produce a list of words with l inserted at the beginning, between all letters and at the end of the word w
          (define (insert-everywhere l w)
            (cond
              [(empty? w) (list (list l))]
              [else (cons (cons l w)
                          (prepend-letter (first w) (insert-everywhere l (rest w))))]))

          ; 1String List-of-words -> List-of-words
          ; produces a list like low, but with l inserted at the beginning, between all letters,
          ; and at the end of all words in the given list
          (define (insert-everywhere/in-all-words l low)
            (cond
              [(empty? low) '()]
              [else (append (insert-everywhere l (first low)) (insert-everywhere/in-all-words l (rest low)))]))

          ; List-of-strings -> List-of-strings
          ; picks out all those Strings that occur in the dictionary
          (define (in-dictionary los)
            (cond
              [(empty? los) '()]
              [else (if (member? (first los) AS-LIST)
                        (cons (first los) (in-dictionary (rest los)))
                        (in-dictionary (rest los)))]))

          ; Word -> List-of-words
          ; creates all rearrangements of the letters in w
          (define (arrangements w)
            (cond
              [(empty? w) (list '())]
              [else (insert-everywhere/in-all-words (first w)
                                                    (arrangements (rest w)))])))

    (in-dictionary
     (words->strings (arrangements (string->word s))))))
