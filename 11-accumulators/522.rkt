;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |522|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Boat is one of:
; - 'left
; - 'right
; interpretation: position of the boat (the left or the right side of the river)

(define-struct mc [lm lc boat rm rc path])
; A PuzzleState is a structure:
;   (make-mc N N Boat N N [List-of PuzzleState])
; interpretation: represents the number of missionaries and cannibals on the left
; and the right sides of the river and the position of the boat
; accumulator: path is the sequence of states traversed to get to the current state

(define MC-INITIAL (make-mc 3 3 'left 0 0 '()))
(define MC-INTERM  (make-mc 2 2 'right 1 1 (list MC-INITIAL)))
(define MC-FINAL   (make-mc 0 0 'right 3 3 (list MC-INITIAL MC-INTERM)))

; PuzzleState -> Boolean
; detects whether in a given state all people are on the right river bank
(check-expect (final? MC-INITIAL) #false)
(check-expect (final? MC-INTERM) #false)
(check-expect (final? MC-FINAL) #true)

(define (final? s)
  (and (= (mc-lm s) 0)
       (= (mc-lc s) 0)
       (symbol=? (mc-boat s) 'right)
       (= (mc-rm s) 3)
       (= (mc-rc s) 3)))

(define RADIUS 5)
(define PADDING-RADIUS 8)
(define PANE-WIDTH (* 4 PADDING-RADIUS))
(define PANE-HEIGHT (* 6 PADDING-RADIUS))

(define MISSIONARY (overlay (circle RADIUS "outline" "black")
                            (circle PADDING-RADIUS "solid" "transparent")))
(define CANNIBAL (overlay (circle RADIUS "solid" "black")
                          (circle PADDING-RADIUS "solid" "transparent")))
(define BOAT (overlay (above (rhombus 6 120 "solid" "black")
                             (rectangle 10 5 "solid" "black"))
                      (square 17 "solid" "transparent")))
(define PANE (empty-scene PANE-WIDTH PANE-HEIGHT))

; N Image -> Image
; draw n instances of img one on top of another
(check-expect (above/n 2 MISSIONARY) (above MISSIONARY MISSIONARY))

(define (above/n n img)
  (cond
    [(zero? n) empty-image]
    [else (above img (above/n (sub1 n) img))]))

; N N -> Image
; draw a pane with m number of missionaries and c number of cannibals
(define (mc-pane m c)
  (overlay (beside/align "middle"
                         (above/n m MISSIONARY)
                         (above/n c CANNIBAL))
           (empty-scene PANE-WIDTH PANE-HEIGHT)))

; Boat -> Image
; draw a pane with the boat
(define (boat-pane b)
  (overlay/align b 'middle BOAT PANE))

; PuzzleState -> Image
; maps a state of the missionary-and-cannibal puzzle to an image
(check-expect (render-mc (make-mc 2 1 'right 1 2 '()))
              (beside (mc-pane 2 1)
                      (boat-pane 'right)
                      (mc-pane 1 2)))

(define (render-mc s)
  (beside (mc-pane (mc-lm s) (mc-lc s))
          (boat-pane (mc-boat s))
          (mc-pane (mc-rm s) (mc-rc s))))
