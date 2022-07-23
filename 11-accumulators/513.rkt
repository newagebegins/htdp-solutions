;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |513|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of:
; – Symbol
; - Lambda
; - Application

(define-struct lmb [para body])
; A Lambda is a structure:
;   (make-lmb Symbol Lam)

(define-struct app [fun arg])
; An Application is a structure:
;   (make-app Lam Lam)

(define ex1 (make-lmb 'x 'x))
(define ex2 (make-lmb 'x 'y))
(define ex3 (make-lmb 'y (make-lmb 'x 'y)))
