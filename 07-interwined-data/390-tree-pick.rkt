;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 390-tree-pick) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct branch [left right])

; A TOS is one of:
; - Symbol
; - (make-branch TOS TOS)

; A Direction is one of:
; - 'left
; - 'right

; A list of Directions is also called a path.

(define INVALID-PATH "invalid path")

;                      B1
;                    /    \
;                  B2      B3
;                 /  \    /  \
;                B4   a  b    c
;               /  \
;              d    e

(define B4 (make-branch 'd 'e))
(define B2 (make-branch B4 'a))
(define B3 (make-branch 'b 'c))
(define B1 (make-branch B2 B3))

; TOS [List-of Direction] -> TOS
; produce the element of the tree of symbols following the given directions
; signal an error when given a symbol and a non-empty path
(check-expect (tree-pick 'hello '()) 'hello)
(check-error (tree-pick 'a '(left)) INVALID-PATH)
(check-expect (tree-pick B4 '()) B4)
(check-expect (tree-pick B4 '(left)) 'd)
(check-expect (tree-pick B4 '(right)) 'e)
(check-expect (tree-pick B2 '(left)) B4)
(check-expect (tree-pick B2 '(right)) 'a)
(check-expect (tree-pick B1 '(left left right)) 'e)
(check-expect (tree-pick B1 '(left left left)) 'd)
(check-expect (tree-pick B1 '(left right)) 'a)
(check-expect (tree-pick B1 '(right right)) 'c)
(check-expect (tree-pick B1 '(right left)) 'b)
(check-error (tree-pick B1 '(left right right)) INVALID-PATH)
(check-error (tree-pick B1 '(right right right)) INVALID-PATH)

(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (empty? lod)) tos]
    [(and (symbol? tos) (cons? lod)) (error INVALID-PATH)]
    [(and (branch? tos) (empty? lod)) tos]
    [(and (branch? tos) (cons? lod)) (tree-pick (if (symbol=? (first lod) 'left)
                                                    (branch-left tos)
                                                    (branch-right tos))
                                                (rest lod))]))
