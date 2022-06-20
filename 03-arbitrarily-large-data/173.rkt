;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |173|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Los is one of:
; - '()
; - (cons String Los)

; An LN is one of:
; - '()
; - (cons Los LN)
; interp. a list of lines, each is a list of strings

(define line-a (cons "a" '()))
(define line-an (cons "an" '()))
(define line-the (cons "the" '()))

(define line0 (cons "foo" (cons "a" (cons "bar" (cons "an" (cons "the" '()))))))
(define line0-na (cons "foo" (cons "bar" '())))
(define line1 (cons "hello" (cons "world" '())))

(define ln-a (cons line-a '()))
(define ln-an (cons line-an '()))
(define ln-the (cons line-the '()))

(define ln0 (cons line0 '()))
(define ln1 (cons line1 '()))
(define ln2 (cons line0 (cons line-a (cons line1 '()))))

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

; String -> Boolean
; determine whether the given string is an article
(check-expect (article? "a") #true)
(check-expect (article? "an") #true)
(check-expect (article? "the") #true)
(check-expect (article? "hello") #false)

(define (article? s)
  (or (string=? s "a")
      (string=? s "an")
      (string=? s "the")))

; Los -> Los
; remove all articles in the line
(check-expect (no-articles-line '()) '())
(check-expect (no-articles-line line-a) '())
(check-expect (no-articles-line line-an) '())
(check-expect (no-articles-line line-the) '())
(check-expect (no-articles-line line0) line0-na)
(check-expect (no-articles-line line1) line1)

(define (no-articles-line los)
  (cond
    [(empty? los) '()]
    [else (if (article? (first los))
              (no-articles-line (rest los))
              (cons (first los) (no-articles-line (rest los))))]))

; LN -> LN
; remove all articles from the list of lines
(check-expect (no-articles '()) '())
(check-expect (no-articles ln-a) (cons '() '()))
(check-expect (no-articles ln-an) (cons '() '()))
(check-expect (no-articles ln-the) (cons '() '()))
(check-expect (no-articles ln0) (cons line0-na '()))
(check-expect (no-articles ln1) ln1)
(check-expect (no-articles ln2) (cons line0-na (cons '() (cons line1 '()))))

(define (no-articles ln)
  (cond
    [(empty? ln) '()]
    [else (cons (no-articles-line (first ln)) (no-articles (rest ln)))]))

; String -> String
; create a new file from f by removing all articles
(define (main f)
  (write-file (string-append "no-articles-" f)
              (collapse (no-articles (read-words/line f)))))
