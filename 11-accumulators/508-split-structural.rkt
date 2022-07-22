;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 508-split-structural) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; [List-of 1String] N -> [List-of 1String]
; returns the first n letters in s
(check-expect (prefix '("1" "2" "3") 0) '())
(check-expect (prefix '("1" "2" "3") 1) '("1"))
(check-expect (prefix '("1" "2" "3") 2) '("1" "2"))
(check-expect (prefix '("1" "2" "3") 3) '("1" "2" "3"))
(check-expect (prefix '("1" "2" "3") 4) '("1" "2" "3"))

(define (prefix s n)
  (cond
    [(or (zero? n) (empty? s)) '()]
    [else (cons (first s) (prefix (rest s) (sub1 n)))]))

; [List-of 1String] N -> [List-of 1String]
; drops the first n letters in s
(check-expect (suffix '("1" "2" "3") 0) '("1" "2" "3"))
(check-expect (suffix '("1" "2" "3") 1) '("2" "3"))
(check-expect (suffix '("1" "2" "3") 2) '("3"))
(check-expect (suffix '("1" "2" "3") 3) '())
(check-expect (suffix '("1" "2" "3") 4) '())

(define (suffix s n)
  (cond
    [(or (zero? n) (empty? s)) s]
    [else (suffix (rest s) (sub1 n))]))

; [List-of 1String] N -> Editor
; split a string into two at the given x
(check-expect (split-structural (explode "123456") 17)
              (make-editor (reverse (explode "12")) (explode "3456")))
(check-expect (split-structural (explode "123456") 18)
              (make-editor (reverse (explode "123")) (explode "456")))
(check-expect (split-structural (explode "123456") 20)
              (make-editor (reverse (explode "123")) (explode "456")))
(check-expect (split-structural (explode "123456") 23)
              (make-editor (reverse (explode "123")) (explode "456")))
(check-expect (split-structural (explode "123456") 24)
              (make-editor (reverse (explode "1234")) (explode "56")))
(check-expect (split-structural (explode "123") 23)
              (make-editor (reverse (explode "123")) '()))
(check-expect (split-structural (explode "123") 30)
              (make-editor (reverse (explode "123")) '()))
(check-expect (split-structural '() 30)
              (make-editor '() '()))
(check-expect (split-structural (explode "123") 6)
              (make-editor (reverse (explode "1")) (explode "23")))
(check-expect (split-structural (explode "123") 5)
              (make-editor '() (explode "123")))
(check-expect (split-structural (explode "123") 0)
              (make-editor '() (explode "123")))

(define (split-structural ed x)
  (cond
    [(empty? ed) (make-editor '() '())]
    [else
     (local (; N [>=1] -> N
             (define (try-prefix n)
               (cond
                 [(= n (length ed)) n]
                 [else (if (> (image-width (editor-text (prefix ed n))) x)
                           (sub1 n)
                           (try-prefix (add1 n)))]))
             ; N
             (define p (try-prefix 1)))
       (make-editor (reverse (prefix ed p))
                    (suffix ed p)))]))
