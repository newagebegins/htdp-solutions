;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |171|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; A List-of-list-of-strings is one of:
; - '()
; - (cons List-of-strings List-of-list-of-strings)

(check-expect (read-words/line "../assets/ttt.txt")
              (cons (cons "TTT" '())
                    (cons '()
                          (cons (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" '())))))
                                (cons (cons "where" (cons "it's" (cons "easy" (cons "to" (cons "see" '())))))
                                      (cons (cons "the" (cons "cryptic" (cons "admonishment" '())))
                                            (cons (cons "T.T.T." '())
                                                  (cons '()
                                                        (cons (cons "When" (cons "you" (cons "feel" (cons "how" (cons "depressingly" '())))))
                                                              (cons (cons "slowly" (cons "you" (cons "climb," '())))
                                                                    (cons (cons "it's" (cons "well" (cons "to" (cons "remember" (cons "that" '())))))
                                                                          (cons (cons "Things" (cons "Take" (cons "Time." '())))
                                                                                (cons '()
                                                                                      (cons (cons "Piet" (cons "Hein" '()))
                                                                                            '()))))))))))))))
