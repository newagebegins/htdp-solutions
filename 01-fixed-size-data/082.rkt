;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |082|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Letter is one of:
; - #false
; - 1String[a-z]
; interp. A lowercase letter or false.

(define-struct word [l1 l2 l3])
; Word is a structure:
;   (make-word Letter Letter Letter)
; interp. A word consisting of 3 letters.

; Word Word -> Word
; compare two words, produce a word that indicates where the given ones agree (letter) and disagree (false).
(check-expect (compare-word (make-word "c" "a" "t") (make-word "c" "a" "r")) (make-word "c" "a" #false))
(check-expect (compare-word (make-word "c" "a" "t") (make-word "c" "a" "t")) (make-word "c" "a" "t"))
(check-expect (compare-word (make-word "c" "a" "t") (make-word "f" "a" "t")) (make-word #false "a" "t"))
(check-expect (compare-word (make-word "c" "a" "t") (make-word "f" "o" "o")) (make-word #false #false #false))
(check-expect (compare-word (make-word "c" "a" "t") (make-word "a" "t" "c")) (make-word #false #false #false))

(define (compare-word w1 w2)
  (make-word (compare-letter (word-l1 w1) (word-l1 w2))
             (compare-letter (word-l2 w1) (word-l2 w2))
             (compare-letter (word-l3 w1) (word-l3 w2))))

; Letter Letter -> Letter
; compare two letters,
; if the letters are equal return this letter, otherwise return false
(check-expect (compare-letter "a" "a") "a")
(check-expect (compare-letter "a" "b") #false)
(check-expect (compare-letter "b" "b") "b")
 
(define (compare-letter l1 l2)
  (if (string=? l1 l2)
      l1
      #false))
