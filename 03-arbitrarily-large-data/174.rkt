;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |174|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Los is one of:
; - '()
; - (cons String Los)

; An LN is one of:
; - '()
; - (cons Los LN)
; interp. a list of lines, each is a list of strings

; Los -> String
; convert a list of strings into a string
(define (los->string los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los)
                         (if (empty? (rest los)) "" " ")
                         (los->string (rest los)))]))

; LN -> String
; converts a list of lines into a string
(define (collapse ln)
  (cond
   [(empty? ln) ""]
   [else (string-append (los->string (first ln))
                        (if (empty? (rest ln)) "" "\n")
                        (collapse (rest ln)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

;;;;;;;;;

(define w0 "x")
(define w0-enc (encode-letter "x"))

(define w1 "ab")
(define w1-enc (string-append (encode-letter "a") (encode-letter "b")))

(define w2 "c")
(define w2-enc (encode-letter "c"))

(define line0 (cons w0 '()))
(define line0-enc (cons w0-enc '()))

(define line1 (cons w1 (cons w2 '())))
(define line1-enc (cons w1-enc (cons w2-enc '())))

(define ln0 (cons line0 (cons line1 '())))

; List-of-1Strings -> List-of-Strings
(check-expect (encode-letters '()) '())
(check-expect (encode-letters (cons "a" (cons "b" '())))
              (cons (encode-letter "a") (cons (encode-letter "b") '())))

(define (encode-letters l)
  (cond
    [(empty? l) '()]
    [else (cons (encode-letter (first l)) (encode-letters (rest l)))]))

; List-of-Strings -> String
; concatenate a list of strings into one string
(check-expect (concat '()) "")
(check-expect (concat (cons "foo" (cons "bar" '()))) "foobar")

(define (concat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (concat (rest l)))]))

; String -> String
; encode a word
(check-expect (encode-word w0) w0-enc)
(check-expect (encode-word w1) w1-enc)
(check-expect (encode-word w2) w2-enc)

(define (encode-word w)
  (concat (encode-letters (explode w))))

; Los -> Los
; encode a line
(check-expect (encode-line '()) '())
(check-expect (encode-line line0) line0-enc)
(check-expect (encode-line line1) line1-enc)

(define (encode-line l)
  (cond
    [(empty? l) '()]
    [else (cons (encode-word (first l))
                (encode-line (rest l)))]))

; LN -> LN
; encode a list of lines
(check-expect (encode '()) '())
(check-expect (encode ln0) (cons line0-enc (cons line1-enc '())))

(define (encode ln)
  (cond
    [(empty? ln) '()]
    [else (cons (encode-line (first ln)) (encode (rest ln)))]))

; String -> String
; create a new file from file f by encoding its contents
(define (encode-file f)
  (write-file (string-append "encoded-" f)
              (collapse (encode (read-words/line f)))))
