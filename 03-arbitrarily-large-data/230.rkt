;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |230|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure:
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of:
; - '()
; - (cons Transition.v3 LOT)
; A Transition.v3 is a structure:
;   (make-transition FSM-State KeyEvent FSM-State)

; FSM-State is a Color.

(define LOT1 (list (make-transition "white" "a" "yellow")
                   (make-transition "yellow" "b" "yellow")
                   (make-transition "yellow" "c" "yellow")
                   (make-transition "yellow" "d" "green")))

(define fsm-abcd (make-fsm "white" LOT1 "green"))
(define fsm-abcd-y (make-fsm "yellow" LOT1 "green"))
(define fsm-abcd-g (make-fsm "green" LOT1 "green"))

; FSM.v2 -> Image
; renders current world state as a colored square

(check-expect (state-as-colored-square fsm-abcd)
              (square 100 "solid" "white"))

(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))

; FSM-State FSM-State -> Boolean
; return #true if two states are equal
(check-expect (state=? "green" "green") #true)
(check-expect (state=? "green" "blue") #false)

(define (state=? s1 s2)
  (string=? s1 s2))

; LOT FSM-State KeyEvent -> FSM-State
; finds the state representing *current* in *transitions*
; and retrieves the *next* field
(check-expect (find LOT1 "white" "a") "yellow")
(check-expect (find LOT1 "white" "x") "white")
(check-expect (find LOT1 "yellow" "b") "yellow")
(check-expect (find LOT1 "yellow" "c") "yellow")
(check-expect (find LOT1 "yellow" "x") "yellow")
(check-expect (find LOT1 "yellow" "d") "green")
(check-expect (find LOT1 "green" "x") "green")

(define (find transitions current ke)
  (cond
    [(empty? transitions) current]
    [else (if (and (state=? (transition-current (first transitions)) current)
                   (key=? (transition-key (first transitions)) ke))
              (transition-next (first transitions))
              (find (rest transitions) current ke))]))

; FSM.v2 KeyEvent -> FSM.v2
; finds the next state from an-fsm and ke

(check-expect (find-next-state fsm-abcd "a") fsm-abcd-y)
(check-expect (find-next-state fsm-abcd "x") fsm-abcd)
(check-expect (find-next-state fsm-abcd-y "b") fsm-abcd-y)
(check-expect (find-next-state fsm-abcd-y "c") fsm-abcd-y)
(check-expect (find-next-state fsm-abcd-y "x") fsm-abcd-y)
(check-expect (find-next-state fsm-abcd-y "d") fsm-abcd-g)

(define (find-next-state an-fsm ke)
  (make-fsm (find (fsm-transitions an-fsm) (fsm-initial an-fsm) ke)
            (fsm-transitions an-fsm)
            (fsm-final an-fsm)))

; FSM.v2 -> Boolean
(check-expect (final-state? fsm-abcd-g) #true)
(check-expect (final-state? fsm-abcd) #false)
(check-expect (final-state? fsm-abcd-y) #false)

(define (final-state? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

; FSM.v2 -> FSM.v2
; run with (fsm-simulate fsm-abcd)
(define (fsm-simulate an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final-state? state-as-colored-square]))
