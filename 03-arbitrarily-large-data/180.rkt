;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |180|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

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

; Lo1s -> String
; convert a list of 1Strings into a string
(check-expect (my-implode '()) "")
(check-expect (my-implode (cons "a" (cons "b" (cons "c" '())))) "abc")

(define (my-implode l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (my-implode (rest l)))]))

; Lo1s -> Image
; renders a list of 1Strings as a text image
(check-expect (editor-text (cons "p" (cons "o" (cons "s" (cons "t" '())))))
              (text "post" FONT-SIZE FONT-COLOR))
(check-expect (editor-text (cons "p" (cons "r" (cons "e" '()))))
              (text "pre" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (text (my-implode s) FONT-SIZE FONT-COLOR))

; Editor -> Image
; renders an editor as an image of the two texts
; separated by the cursor
(check-expect (editor-render (create-editor "foo" "bar"))
              (place-image/align
               (beside
                (text "foo" FONT-SIZE FONT-COLOR)
                CURSOR
                (text "bar" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))

(define (editor-render e)
  (place-image/align
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))

; Editor -> Editor
; moves the cursor position one 1String left, if possible
(check-expect (editor-lft (make-editor '() '()))
              (make-editor '() '()))

; ab|cd -> a|bcd
(check-expect (editor-lft (make-editor (cons "b" (cons "a" '()))
                                       (cons "c" (cons "d" '()))))
              (make-editor (cons "a" '())
                           (cons "b" (cons "c" (cons "d" '())))))

; |cd -> |cd
(check-expect (editor-lft (make-editor '() (cons "c" (cons "d" '()))))
              (make-editor '() (cons "c" (cons "d" '()))))

; ab| -> a|b
(check-expect (editor-lft (make-editor (cons "b" (cons "a" '())) '()))
              (make-editor (cons "a" '()) (cons "b" '())))

(define (editor-lft ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed)) (editor-post ed)))))

; Editor -> Editor
; moves the cursor position one 1String right, if possible

; | -> |
(check-expect (editor-rgt (make-editor '() '())) (make-editor '() '()))

; ab|cd -> abc|d
(check-expect (editor-rgt (make-editor (cons "b" (cons "a" '()))
                                       (cons "c" (cons "d" '()))))
              (make-editor (cons "c" (cons "b" (cons "a" '())))
                           (cons "d" '())))

; ab| -> ab|
(check-expect (editor-rgt (make-editor (cons "a" (cons "b" '())) '()))
              (make-editor (cons "a" (cons "b" '())) '()))

; |abc -> a|bc
(check-expect (editor-rgt (make-editor '() (cons "a" (cons "b" (cons "c" '())))))
              (make-editor (cons "a" '()) (cons "b" (cons "c" '()))))

(define (editor-rgt ed)
  (if (empty? (editor-post ed))
      ed
      (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                   (rest (editor-post ed)))))

; Editor -> Editor
; deletes a 1String to the left of the cursor, if possible

; | -> |
(check-expect (editor-del (make-editor '() '()))
              (make-editor '() '()))

; abc|de -> ab|cd
(check-expect (editor-del (make-editor (cons "c" (cons "b" (cons "a" '())))
                                       (cons "c" (cons "d" '()))))
              (make-editor (cons "b" (cons "a" '()))
                           (cons "c" (cons "d" '()))))

; |a -> |a
(check-expect (editor-del (make-editor '() (cons "a" '())))
              (make-editor '() (cons "a" '())))

; abc| -> ab|
(check-expect (editor-del (make-editor (cons "c" (cons "b" (cons "a" '()))) '()))
              (make-editor (cons "b" (cons "a" '())) '()))

(define (editor-del ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed)) (editor-post ed))))

; Editor 1String -> Editor
; insert the 1String k between pre and post
(check-expect (editor-ins (make-editor '() '()) "e")
              (make-editor (cons "e" '()) '()))

(check-expect (editor-ins (make-editor (cons "d" '()) (cons "f" (cons "g" '()))) "e")
              (make-editor (cons "e" (cons "d" '())) (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed)) (editor-post ed)))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))

(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "a" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "a" "foo") "\b")
              (create-editor "" "foo"))
(check-expect (editor-kh (create-editor "cd" "") "\b")
              (create-editor "c" ""))
(check-expect (editor-kh (create-editor "ab" "cd") "\b")
              (create-editor "a" "cd"))
(check-expect (editor-kh (create-editor "" "cd") "\b")
              (create-editor "" "cd"))

(check-expect (editor-kh (create-editor "" "") "left")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "hello") "left")
              (create-editor "" "hello"))
(check-expect (editor-kh (create-editor "foo" "bar") "left")
              (create-editor "fo" "obar"))
(check-expect (editor-kh (create-editor "abc" "") "left")
              (create-editor "ab" "c"))

(check-expect (editor-kh (create-editor "" "") "right")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "abc" "") "right")
              (create-editor "abc" ""))
(check-expect (editor-kh (create-editor "" "abc") "right")
              (create-editor "a" "bc"))
(check-expect (editor-kh (create-editor "cde" "fgh") "right")
              (create-editor "cdef" "gh"))

(check-expect (editor-kh (create-editor "cde" "fgh") "up")
              (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "cde" "fgh") "down")
              (create-editor "cde" "fgh"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))
