;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |109|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; ExpectsToSee is one of:
; - AA
; - BB
; - DD
; - ER

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c' or 'd'")
(define DD "finished")
(define ER "error, illegal key")

(define SIDE 100)

; ExpectsToSee -> Image
; for AA show white rectangle, for BB - yellow, for DD - green, for ER - red
(check-expect (render AA) (square SIDE "solid" "white"))
(check-expect (render BB) (square SIDE "solid" "yellow"))
(check-expect (render DD) (square SIDE "solid" "green"))
(check-expect (render ER) (square SIDE "solid" "red"))

(define (render ets)
  (square
   SIDE
   "solid"
   (cond
     [(equal? ets AA) "white"]
     [(equal? ets BB) "yellow"]
     [(equal? ets DD) "green"]
     [(equal? ets ER) "red"])))

; ExpectsToSee KeyEvent -> ExpectsToSee
(check-expect (handle-keys AA "a") BB)
(check-expect (handle-keys AA "b") ER)
(check-expect (handle-keys AA "d") ER)

(check-expect (handle-keys BB "b") BB)
(check-expect (handle-keys BB "c") BB)
(check-expect (handle-keys BB "d") DD)
(check-expect (handle-keys BB "a") ER)
(check-expect (handle-keys BB "left") ER)

(check-expect (handle-keys DD "a") DD)
(check-expect (handle-keys DD "d") DD)

(check-expect (handle-keys ER "a") ER)
(check-expect (handle-keys ER "b") ER)

(define (handle-keys ets ke)
  (cond
    [(equal? ets AA)
     (if (key=? ke "a")
         BB
         ER)]
    [(equal? ets BB)
     (cond
       [(or (key=? ke "b") (key=? ke "c")) BB]
       [(key=? ke "d") DD]
       [else ER])]
    [(equal? ets DD) DD]
    [(equal? ets ER) ER]))

; ExpectsToSee -> ExpectsToSee
; run with (main AA)
(define (main ets)
  (big-bang ets
    [to-draw render]
    [on-key handle-keys]))
