;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 400-dna-prefix-and-delta) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A DNAsym is one of:
; - 'a
; - 'c
; - 'g
; - 't

(define OUT-OF-LETTERS "out of letters")

; [List-of DNAsym] [List-of DNAsym] -> Boolean
; returns #true if the pattern p is identical to the initial part of the search string s
(check-expect (DNAprefix '(a) '(a)) #true)
(check-expect (DNAprefix '(a) '(c)) #false)
(check-expect (DNAprefix '(t) '(t c)) #true)
(check-expect (DNAprefix '(t c) '(t c)) #true)
(check-expect (DNAprefix '(g) '(t g)) #false)
(check-expect (DNAprefix '(t c) '(t g)) #false)
(check-expect (DNAprefix '(a a c) '(a a c g t)) #true)
(check-expect (DNAprefix '(a a c) '(a a a g t)) #false)
(check-expect (DNAprefix '(a a c) '(a a)) #false)

(define (DNAprefix p s)
  (cond
    [(empty? p) #true]
    [(empty? s) #false]
    [else (and (symbol=? (first p) (first s))
               (DNAprefix (rest p) (rest s)))]))

; [List-of DNAsym] [List-of DNAsym] -> [Maybe DNAsym]
; returns the first item in the search string s beyond the pattern p
; signals an error if the lists are identical and there is no DNA letter beyond the pattern
; returns #false if the pattern does not match the beginning of the search string
(check-error (DNAdelta '(a) '(a)) OUT-OF-LETTERS)
(check-expect (DNAdelta '(a) '(c)) #false)
(check-expect (DNAdelta '(t) '(t c)) 'c)
(check-error (DNAdelta '(t c) '(t c)) OUT-OF-LETTERS)
(check-expect (DNAdelta '(g) '(t g)) #false)
(check-expect (DNAdelta '(t c) '(t g)) #false)
(check-expect (DNAdelta '(a a c) '(a a c g t)) 'g)
(check-expect (DNAdelta '(a a c) '(a a a g t)) #false)
(check-expect (DNAdelta '(a a c) '(a a)) #false)

(define (DNAdelta p s)
  (cond
    [(and (empty? p) (empty? s)) (error OUT-OF-LETTERS)]
    [(and (empty? p) (cons? s)) (first s)]
    [(and (cons? p) (empty? s)) #false]
    [(and (cons? p) (cons? s)) (if (symbol=? (first p) (first s))
                                   (DNAdelta (rest p) (rest s))
                                   #false)]))
