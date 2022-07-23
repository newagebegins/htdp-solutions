;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 509-split) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")

; [List-of 1String] -> Image
; renders a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation: if (make-editor p s) is the state of an interactive editor,
; (reverse p) corresponds to the text to the left of the cursor and s to the text on the right

; [List-of 1String] N -> Editor
; split a string into two at the given x
(check-expect (split (explode "123456") 17)
              (make-editor (reverse (explode "12")) (explode "3456")))
(check-expect (split (explode "123456") 18)
              (make-editor (reverse (explode "123")) (explode "456")))
(check-expect (split (explode "123456") 20)
              (make-editor (reverse (explode "123")) (explode "456")))
(check-expect (split (explode "123456") 23)
              (make-editor (reverse (explode "123")) (explode "456")))
(check-expect (split (explode "123456") 24)
              (make-editor (reverse (explode "1234")) (explode "56")))
(check-expect (split (explode "123") 23)
              (make-editor (reverse (explode "123")) '()))
(check-expect (split (explode "123") 30)
              (make-editor (reverse (explode "123")) '()))
(check-expect (split '() 30)
              (make-editor '() '()))
(check-expect (split (explode "123") 6)
              (make-editor (reverse (explode "1")) (explode "23")))
(check-expect (split (explode "123") 5)
              (make-editor '() (explode "123")))
(check-expect (split (explode "123") 0)
              (make-editor '() (explode "123")))

(define (split ed x)
  (local (; [List-of 1String] [List-of 1String] -> Editor
          (define (split/a p s)
            (cond
              [(empty? s) (make-editor p s)]
              [else
               (local (; [List-of 1String]
                       (define next-p (cons (first s) p))
                       ; [List-of 1String]
                       (define next-s (rest s)))
                 (if (> (image-width (editor-text (reverse next-p))) x)
                     (make-editor p s)
                     (split/a next-p next-s)))])))
    (split/a '() ed)))
