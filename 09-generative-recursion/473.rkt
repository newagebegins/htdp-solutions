;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |473|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define cyclic-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'B 'D)
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

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'A 'G sample-graph) '(A B E F G))
(check-expect (find-path 'A 'B sample-graph) '(A B))
(check-expect (find-path 'B 'A sample-graph) #false)
(check-expect (find-path 'B 'C cyclic-graph) '(B E C))

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; [X] [List-of X] -> [List of (list X X)]
(check-expect (create-pairs '(A B C D)) '((A B) (A C) (A D) (B C) (B D) (C D)))

(define (create-pairs l)
  (cond
    [(empty? l) '()]
    [else (append (map (lambda (x) (list (first l) x)) (rest l))
                  (create-pairs (rest l)))]))

; Graph -> Boolean
; return #true if there is a path between every pair of nodes
(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes '((A B) (B C) (C))) #true)
(check-expect (test-on-all-nodes '((A B C) (B C) (C))) #true)
(check-expect (test-on-all-nodes '((A B C) (B) (C))) #false)
; infinite loop:
;(check-expect (test-on-all-nodes cyclic-graph) #false)

(define (test-on-all-nodes g)
  (local ((define nodes (map first g))
          (define pairs (create-pairs nodes)))
    (andmap (lambda (p)
              (or (cons? (find-path (first p) (second p) g))
                  (cons? (find-path (second p) (first p) g))))
            pairs)))
