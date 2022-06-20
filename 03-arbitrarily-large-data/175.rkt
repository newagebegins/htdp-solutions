;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |175|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define-struct wc-result [chars words lines])
; A WCResult is a structure:
;   (make-wc-result Number Number Number)
; interp. represents a number of characters, words and lines in a file

; String -> WCResult
; count characters, words and lines in a given file
(define (wc f)
  (make-wc-result
   (length (read-1strings f))
   (length (read-words f))
   (length (read-lines f))))
