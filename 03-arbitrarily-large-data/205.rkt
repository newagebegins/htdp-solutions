;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |205|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; LAssoc
(define T1 (list (list "name" "Track #1")
                 (list "album" "Album #1")
                 (list "track#" 3)
                 (list "added" (create-date 2000 1 1 1 1 1))))

(define T2 (list (list "name" "Track #2")
                 (list "album" "Album #2")
                 (list "track#" 1)
                 (list "added" (create-date 2001 1 1 1 1 1))))

; LLists
(define TL1 (list T1 T2))
