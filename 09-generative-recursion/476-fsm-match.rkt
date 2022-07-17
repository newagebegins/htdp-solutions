;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 476-fsm-match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])

; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.

(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean
; does an-fsm recognize the given string
(check-expect (fsm-match? fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abbcccd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "da") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)
(check-expect (fsm-match? fsm-a-bc*-d "a") #false)
(check-expect (fsm-match? fsm-a-bc*-d "abc") #false)
(check-expect (fsm-match? fsm-a-bc*-d "bcd") #false)

(define (fsm-match? an-fsm a-string)
  (local (; FSM-State [List-of 1String] -> Boolean
          (define (match cur-state l)
            (cond
              [(empty? l) #false]
              [else (local (; [Maybe FSM-State]
                            (define new-state (transition cur-state (first l))))
                      (if (string? new-state)
                          (if (string=? new-state final-state)
                              #true
                              (match new-state (rest l)))
                          #false))]))
          ; FSM-State 1String -> [Maybe FSM-State]
          (define (transition cur-state letter)
            (local (; [List-of 1Transition] -> [Maybe FSM-State]
                    (define (find lot)
                      (cond
                        [(empty? lot) #false]
                        [else (local (; 1Transition
                                      (define t (first lot)))
                                (if (and (string=? (transition-current t) cur-state)
                                         (string=? (transition-key t) letter))
                                    (transition-next t)
                                    (find (rest lot))))])))
              (find transitions)))
          ; FSM-State
          (define final-state (fsm-final an-fsm))
          ; [List-of 1Transition]
          (define transitions (fsm-transitions an-fsm)))
    (match (fsm-initial an-fsm) (explode a-string))))
