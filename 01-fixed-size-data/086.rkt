;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |086|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; String -> String
; extracts the first character from a string s
(check-expect (string-first "xyz") "x")
(check-expect (string-first "") "")

(define (string-first s)
  (if (> (string-length s) 0)
      (substring s 0 1)
      ""))

;; String -> String
;; extract the last character from a string s
(check-expect (string-last "abc") "c")
(check-expect (string-last "") "")

(define (string-last s)
  (if (> (string-length s) 0)
      (substring s (sub1 (string-length s)))
      ""))

;; String -> String
;; removes the first character from a given string
(check-expect (string-remove-first "abc") "bc")
(check-expect (string-remove-first "") "")

(define (string-remove-first s)
  (if (> (string-length s) 0)
      (substring s 1)
      ""))

;; String -> String
;; removes the last character from a given string
(check-expect (string-remove-last "abc") "ab")
(check-expect (string-remove-last "") "")

(define (string-remove-last s)
  (if (> (string-length s) 0)
      (substring s 0 (sub1 (string-length s)))
      ""))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interp. (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define WIDTH 200) ; scene width
(define HEIGHT 20) ; scene and cursor height
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")
(define MTS (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; render the text being edited and the cursor without the background
(check-expect (render-text (make-editor "hello" "world"))
              (beside (text "hello" TEXT-SIZE TEXT-COLOR)
                      CURSOR
                      (text "world" TEXT-SIZE TEXT-COLOR)))

(define (render-text ed)
  (beside (text (editor-pre ed) TEXT-SIZE TEXT-COLOR)
           CURSOR
           (text (editor-post ed) TEXT-SIZE TEXT-COLOR)))

; Editor -> Boolean
; check if resultant text image is wider then the background image
(check-expect (out-of-bounds? (make-editor "12345678901234567890123" "")) #true)
(check-expect (out-of-bounds? (make-editor "1234567890123456789012" "")) #false)

(define (out-of-bounds? ed)
  (> (image-width (render-text ed)) WIDTH))

; Editor -> Image
; render the text being edited and the cursor with the background
(check-expect (render (make-editor "hello" "world"))
              (overlay/align
               "left" "center"
               (beside (text "hello" TEXT-SIZE TEXT-COLOR)
                       CURSOR
                       (text "world" TEXT-SIZE TEXT-COLOR))
               MTS))
(check-expect (render (make-editor "" ""))
              (overlay/align
               "left" "center"
               CURSOR
               MTS))
(check-expect (render (make-editor "" "world"))
              (overlay/align
               "left" "center"
               (beside CURSOR
                       (text "world" TEXT-SIZE TEXT-COLOR))
               MTS))
(check-expect (render (make-editor "hello" ""))
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

; Editor KeyEvent -> Editor
(check-expect (edit (make-editor "" "") "a") (make-editor "a" ""))
(check-expect (edit (make-editor "a" "") "b") (make-editor "ab" ""))

(check-expect (edit (make-editor "ab" "") "\b") (make-editor "a" ""))
(check-expect (edit (make-editor "" "") "\b") (make-editor "" ""))
(check-expect (edit (make-editor "ab" "cd") "\b") (make-editor "a" "cd"))
(check-expect (edit (make-editor "a" "cd") "\b") (make-editor "" "cd"))

(check-expect (edit (make-editor "a" "") "\t") (make-editor "a" ""))
(check-expect (edit (make-editor "a" "") "\r") (make-editor "a" ""))
(check-expect (edit (make-editor "" "a") "\t") (make-editor "" "a"))
(check-expect (edit (make-editor "" "a") "\r") (make-editor "" "a"))

(check-expect (edit (make-editor "ab" "cd") "right") (make-editor "abc" "d"))
(check-expect (edit (make-editor "abc" "d") "left") (make-editor "ab" "cd"))
(check-expect (edit (make-editor "" "a") "left") (make-editor "" "a"))
(check-expect (edit (make-editor "a" "") "right") (make-editor "a" ""))
(check-expect (edit (make-editor "" "") "left") (make-editor "" ""))
(check-expect (edit (make-editor "" "") "right") (make-editor "" ""))

(check-expect (edit (make-editor "a" "b") "shift") (make-editor "a" "b"))

(check-expect (edit (make-editor "1234567890123456789012" "") "3") (make-editor "1234567890123456789012" ""))

(define (edit ed ke)
  (cond
    [(key=? ke "left") (make-editor (string-remove-last (editor-pre ed))
                                    (string-append (string-last (editor-pre ed))
                                                   (editor-post ed)))]
    [(key=? ke "right") (make-editor (string-append (editor-pre ed)
                                                    (string-first (editor-post ed)))
                                     (string-remove-first (editor-post ed)))]
    [(key=? ke "\b") (make-editor (string-remove-last (editor-pre ed))
                                  (editor-post ed))]
    [(or (key=? ke "\t") (key=? ke "\r")) ed]
    [(= (string-length ke) 1)
     (if (out-of-bounds? (make-editor (string-append (editor-pre ed) ke) (editor-post ed)))
         ed
         (make-editor (string-append (editor-pre ed) ke) (editor-post ed)))]
    [else ed]))

; String -> Editor
; run the program with (run "")
(define (run pre)
  (big-bang (make-editor pre "")
    [to-draw render]
    [on-key edit]))
