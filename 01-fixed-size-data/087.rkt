;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |087|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [text cursor])
; An Editor is a structure:
;   (make-editor String Number)
; interp. (make-editor t c) describes an editor
; whose visible text is t with the cursor displayed after c characters from the left

(define WIDTH 200) ; scene width
(define HEIGHT 20) ; scene and cursor height
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")
(define MTS (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; render the text being edited and the cursor without the background
(check-expect (render-text (make-editor "" 0)) CURSOR)
(check-expect (render-text (make-editor "helloworld" 5))
              (beside (text "hello" TEXT-SIZE TEXT-COLOR)
                      CURSOR
                      (text "world" TEXT-SIZE TEXT-COLOR)))

(define (render-text ed)
  (beside (text (substring (editor-text ed) 0 (editor-cursor ed)) TEXT-SIZE TEXT-COLOR)
           CURSOR
           (text (substring (editor-text ed) (editor-cursor ed)) TEXT-SIZE TEXT-COLOR)))

; Editor Editor -> Editor
; ensure the new editor text image is not wider then the background image, otherwise keep old editor
(check-expect (ensure-in-bounds (make-editor "12345678901234567890123" 23) (make-editor "1234567890123456789012" 22)) (make-editor "1234567890123456789012" 22))
(check-expect (ensure-in-bounds (make-editor "1234567890123456789012" 22) (make-editor "123456789012345678901" 21)) (make-editor "1234567890123456789012" 22))

(define (ensure-in-bounds new old)
  (if (> (image-width (render-text new)) WIDTH)
      old
      new))

; Editor -> Image
; render the text being edited and the cursor with the background
(check-expect (render (make-editor "helloworld" 5))
              (overlay/align
               "left" "center"
               (beside (text "hello" TEXT-SIZE TEXT-COLOR)
                       CURSOR
                       (text "world" TEXT-SIZE TEXT-COLOR))
               MTS))
(check-expect (render (make-editor "" 0))
              (overlay/align
               "left" "center"
               CURSOR
               MTS))
(check-expect (render (make-editor "world" 0))
              (overlay/align
               "left" "center"
               (beside CURSOR
                       (text "world" TEXT-SIZE TEXT-COLOR))
               MTS))
(check-expect (render (make-editor "hello" 5))
              (overlay/align
               "left" "center"
               (beside (text "hello" TEXT-SIZE TEXT-COLOR)
                       CURSOR)
               MTS))

(define (render ed)
  (overlay/align
   "left" "center"
   (render-text ed)
   MTS))

; Editor -> Editor
; move cursor to the left
(check-expect (cursor-left (make-editor "abcd" 3)) (make-editor "abcd" 2))
(check-expect (cursor-left (make-editor "a" 1)) (make-editor "a" 0))
(check-expect (cursor-left (make-editor "a" 0)) (make-editor "a" 0))
(check-expect (cursor-left (make-editor "" 0)) (make-editor "" 0))

(define (cursor-left ed)
  (make-editor (editor-text ed)
               (max (sub1 (editor-cursor ed)) 0)))

; Editor -> Editor
; move cursor to the right
(check-expect (cursor-right (make-editor "abcd" 2)) (make-editor "abcd" 3))
(check-expect (cursor-right (make-editor "abcd" 4)) (make-editor "abcd" 4))
(check-expect (cursor-right (make-editor "a" 0)) (make-editor "a" 1))
(check-expect (cursor-right (make-editor "a" 1)) (make-editor "a" 1))
(check-expect (cursor-right (make-editor "" 0)) (make-editor "" 0))

(define (cursor-right ed)
  (make-editor (editor-text ed)
               (min (add1 (editor-cursor ed)) (string-length (editor-text ed)))))

; Editor -> Editor
; delete a letter in the editor
(check-expect (backspace (make-editor "ab" 2)) (make-editor "a" 1))
(check-expect (backspace (make-editor "" 0)) (make-editor "" 0))
(check-expect (backspace (make-editor "abcd" 2)) (make-editor "acd" 1))
(check-expect (backspace (make-editor "abcd" 4)) (make-editor "abc" 3))
(check-expect (backspace (make-editor "acd" 1)) (make-editor "cd" 0))

(define (backspace ed)
  (if (> (editor-cursor ed) 0)
      (make-editor (string-append (substring (editor-text ed) 0 (sub1 (editor-cursor ed)))
                                  (substring (editor-text ed) (editor-cursor ed)))
                   (sub1 (editor-cursor ed)))
      ed))

; Editor 1String -> Editor
; insert a letter into the editor
(check-expect (insert-letter (make-editor "" 0) "a") (make-editor "a" 1))
(check-expect (insert-letter (make-editor "a" 1) "b") (make-editor "ab" 2))
(check-expect (insert-letter (make-editor "ab" 1) "c") (make-editor "acb" 2))
(check-expect (insert-letter (make-editor "1234567890123456789012" 22) "3") (make-editor "1234567890123456789012" 22))

(define (insert-letter ed l)
  (ensure-in-bounds (make-editor (string-append (substring (editor-text ed) 0 (editor-cursor ed))
                                                l
                                                (substring (editor-text ed) (editor-cursor ed)))
                                 (add1 (editor-cursor ed)))
                    ed))
  

; Editor KeyEvent -> Editor
(check-expect (edit (make-editor "" 0) "a") (make-editor "a" 1))
(check-expect (edit (make-editor "a" 1) "b") (make-editor "ab" 2))
(check-expect (edit (make-editor "ab" 1) "c") (make-editor "acb" 2))

(check-expect (edit (make-editor "ab" 2) "\b") (make-editor "a" 1))
(check-expect (edit (make-editor "" 0) "\b") (make-editor "" 0))
(check-expect (edit (make-editor "abcd" 2) "\b") (make-editor "acd" 1))
(check-expect (edit (make-editor "acd" 1) "\b") (make-editor "cd" 0))

(check-expect (edit (make-editor "a" 1) "\t") (make-editor "a" 1))
(check-expect (edit (make-editor "a" 1) "\r") (make-editor "a" 1))
(check-expect (edit (make-editor "a" 0) "\t") (make-editor "a" 0))
(check-expect (edit (make-editor "a" 0) "\r") (make-editor "a" 0))

(check-expect (edit (make-editor "abcd" 2) "right") (make-editor "abcd" 3))
(check-expect (edit (make-editor "abcd" 3) "left") (make-editor "abcd" 2))
(check-expect (edit (make-editor "a" 0) "left") (make-editor "a" 0))
(check-expect (edit (make-editor "a" 1) "right") (make-editor "a" 1))
(check-expect (edit (make-editor "" 0) "left") (make-editor "" 0))
(check-expect (edit (make-editor "" 0) "right") (make-editor "" 0))

(check-expect (edit (make-editor "ab" 1) "shift") (make-editor "ab" 1))

(check-expect (edit (make-editor "1234567890123456789012" 22) "3") (make-editor "1234567890123456789012" 22))

(define (edit ed ke)
  (cond
    [(key=? ke "left") (cursor-left ed)]
    [(key=? ke "right") (cursor-right ed)]
    [(key=? ke "\b") (backspace ed)]
    [(or (key=? ke "\t") (key=? ke "\r")) ed]
    [(= (string-length ke) 1) (insert-letter ed ke)]
    [else ed]))

; String -> Editor
; run the program with (run "")
(define (run text)
  (big-bang (make-editor text 0)
    [to-draw render]
    [on-key edit]))
