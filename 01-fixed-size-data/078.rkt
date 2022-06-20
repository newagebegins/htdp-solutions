;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |078|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Letter is one of:
; - #false
; - 1String[a-z]

(define-struct 3-letter-word [a b c])
; 3LetterWord is a structure:
;   (make-3-letter-word Letter Letter Letter)
