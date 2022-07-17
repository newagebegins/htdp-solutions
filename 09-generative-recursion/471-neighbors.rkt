;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 471-neighbors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Node is a Symbol

; A Graph is a [List-of [List-of Node]]

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

; Node Graph -> [List-of Node]
; produce a list of immediate neighbors of n in g
(check-expect (neighbors 'A sample-graph) (list 'B 'E))
(check-expect (neighbors 'B sample-graph) (list 'E 'F))
(check-expect (neighbors 'C sample-graph) (list 'D))
(check-expect (neighbors 'D sample-graph) '())

(define (neighbors n g)
  (cond
    [(empty? g) '()]
    [else (local ((define l (first g)))
            (if (symbol=? (first l) n)
                (rest l)
                (neighbors n (rest g))))]))
