;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |172|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Los is one of:
; - '()
; - (cons String Los)

; An LN is one of:
; - '()
; - (cons Los LN)
; interp. a list of lines, each is a list of strings

(define line0 '())
(define line1 (cons "foo" '()))
(define line2 (cons "hello" (cons "world" '())))

(define ln0 '())
(define ln1 (cons line0 '()))
(define ln2 (cons line1 '()))
(define ln3 (cons line2 '()))
(define ln4 (cons line1 (cons line2 '())))
(define ln5 (cons line1 (cons line0 (cons line2 '()))))

; Los -> String
; convert a list of strings into a string
(check-expect (los->string line0) "")
(check-expect (los->string line1) "foo")
(check-expect (los->string line2) "hello world")

(define (los->string los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los)
                         (if (empty? (rest los)) "" " ")
                         (los->string (rest los)))]))

; LN -> String
; converts a list of lines into a string
(check-expect (collapse ln0) "")
(check-expect (collapse ln1) "")
(check-expect (collapse ln2) "foo")
(check-expect (collapse ln3) "hello world")
(check-expect (collapse ln4) "foo\nhello world")
(check-expect (collapse ln5) "foo\n\nhello world")

(define (collapse ln)
  (cond
   [(empty? ln) ""]
   [else (string-append (los->string (first ln))
                        (if (empty? (rest ln)) "" "\n")
                        (collapse (rest ln)))]))
