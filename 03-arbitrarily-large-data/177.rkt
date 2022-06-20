;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |177|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1s Lo1s)

; An Lo1s is one of:
; - '()
; - (cons 1String Lo1s)

; String String -> Editor
; create an editor given text to the left and to the right of the cursor
(check-expect (create-editor "" "") (make-editor '() '()))
(check-expect (create-editor "ab" "cd") (make-editor (cons "b" (cons "a" '())) (cons "c" (cons "d" '()))))

(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))
