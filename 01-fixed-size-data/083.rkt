;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |083|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interp. (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define HEIGHT 20) ; scene and cursor height
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")
(define MTS (empty-scene 200 HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; render the text being edited and the cursor
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
   (beside (text (editor-pre ed) TEXT-SIZE TEXT-COLOR)
           CURSOR
           (text (editor-post ed) TEXT-SIZE TEXT-COLOR))
   MTS))
