;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |229|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct ktransition [current key next])
; A Transition is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

; FSM-State is a Color.

; interpretation: An FSM represents the transitions that a finite state machine can take from one
; state to another in reaction to keystrokes

(define-struct fs [fsm current])
; A SimulationState is a structure:
;   (make-fs FSM FSM-State)

(define fsm-abcd
  (list (make-ktransition "white" "a" "yellow")
        (make-ktransition "yellow" "b" "yellow")
        (make-ktransition "yellow" "c" "yellow")
        (make-ktransition "yellow" "d" "green")))

; SimulationState -> Image
; renders current world state as a colored square

(check-expect (state-as-colored-square (make-fs fsm-abcd "white"))
              (square 100 "solid" "white"))

(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; FSM-State FSM-State -> Boolean
; return #true if two states are equal
(check-expect (state=? "green" "green") #true)
(check-expect (state=? "green" "blue") #false)

(define (state=? s1 s2)
  (string=? s1 s2))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing *current* in *transitions*
; and retrieves the *next* field
(check-expect (find fsm-abcd "white" "a") "yellow")
(check-expect (find fsm-abcd "white" "x") "white")
(check-expect (find fsm-abcd "yellow" "b") "yellow")
(check-expect (find fsm-abcd "yellow" "c") "yellow")
(check-expect (find fsm-abcd "yellow" "x") "yellow")
(check-expect (find fsm-abcd "yellow" "d") "green")
(check-expect (find fsm-abcd "green" "x") "green")

(define (find transitions current ke)
  (cond
    [(empty? transitions) current]
    [else (if (and (state=? (ktransition-current (first transitions)) current)
                   (key=? (ktransition-key (first transitions)) ke))
              (ktransition-next (first transitions))
              (find (rest transitions) current ke))]))

; SimulationState KeyEvent -> SimulationState
; finds the next state from an-fsm and ke

(check-expect
 (find-next-state (make-fs fsm-abcd "white") "a")
 (make-fs fsm-abcd "yellow"))
(check-expect
 (find-next-state (make-fs fsm-abcd "white") "x")
 (make-fs fsm-abcd "white"))
(check-expect
 (find-next-state (make-fs fsm-abcd "yellow") "b")
 (make-fs fsm-abcd "yellow"))
(check-expect
 (find-next-state (make-fs fsm-abcd "yellow") "c")
 (make-fs fsm-abcd "yellow"))
(check-expect
 (find-next-state (make-fs fsm-abcd "yellow") "x")
 (make-fs fsm-abcd "yellow"))
(check-expect
 (find-next-state (make-fs fsm-abcd "yellow") "d")
 (make-fs fsm-abcd "green"))
(check-expect
 (find-next-state (make-fs fsm-abcd "green") "x")
 (make-fs fsm-abcd "green"))

(define (find-next-state an-fsm ke)
  (make-fs (fs-fsm an-fsm)
           (find (fs-fsm an-fsm) (fs-current an-fsm) ke)))

; FSM FSM-State -> SimulationState
; match the keys pressed with the given FSM
; run with: (simulate fsm-abcd "white")
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))
