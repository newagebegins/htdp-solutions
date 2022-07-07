;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 396-hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

; HM-Word HM-Word Letter -> HM-Word
; replace "_" with l in s if l is present in w (the word to be guessed)
(check-expect (compare-word (explode "hello") (explode "_____") "h") (explode "h____"))
(check-expect (compare-word (explode "hello") (explode "h____") "o") (explode "h___o"))
(check-expect (compare-word (explode "hello") (explode "h___o") "l") (explode "h_llo"))
(check-expect (compare-word (explode "hello") (explode "h_llo") "e") (explode "hello"))
(check-expect (compare-word (explode "hello") (explode "h____") "w") (explode "h____"))
(check-expect (compare-word (explode "world") (explode "_____") "d") (explode "____d"))
(check-expect (compare-word '() '() "h") '())

(define (compare-word w s l)
  (cond
    [(empty? w) '()]
    [else (cons (if (string=? (first w) l)
                    l
                    (first s))
                (compare-word (rest w) (rest s) l))]))

; String N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))
