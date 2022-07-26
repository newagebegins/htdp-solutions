;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 524-solve) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

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

(define S1 (make-mc 3 3 'left 0 0 '()))

(define S2 (make-mc 3 2 'right 0 1 (list S1)))
(define S3 (make-mc 2 2 'right 1 1 (list S1)))
(define S4 (make-mc 3 1 'right 0 2 (list S1)))

(define S5 (make-mc 3 2 'left 0 1 (list S3 S1)))
(define S6 (make-mc 3 2 'left 0 1 (list S4 S1)))

(define S7 (make-mc 2 2 'right 1 1 (list S6 S4 S1)))
(define S8 (make-mc 3 0 'right 0 3 (list S6 S4 S1)))

; PuzzleState PuzzleState -> Boolean
; check whether the two states are the same (disregarding paths)
(check-expect (mc=? (make-mc 3 3 'left 0 0 (list (make-mc 3 3 'left 0 0 '())))
                    (make-mc 3 3 'left 0 0 '()))
              #true)
(check-expect (mc=? (make-mc 3 2 'left 0 1 (list (make-mc 3 3 'left 0 0 '())))
                    (make-mc 3 3 'left 0 0 '()))
              #false)
(check-expect (mc=? (make-mc 3 3 'left 0 0 '())
                    (make-mc 3 3 'right 0 0 '()))
              #false)

(define (mc=? s1 s2)
  (and (= (mc-lm s1) (mc-lm s2))
       (= (mc-lc s1) (mc-lc s2))
       (symbol=? (mc-boat s1) (mc-boat s2))
       (= (mc-rm s1) (mc-rm s2))
       (= (mc-rc s1) (mc-rc s2))))

; PuzzleState -> Boolean
; return #true if the state is the same as one of the states in its path
(check-expect (already-traversed? S1) #false)
(check-expect (already-traversed? S2) #false)
(check-expect (already-traversed? S6) #false)
(check-expect (already-traversed? S7) #false)

(check-expect (already-traversed? (make-mc 3 3 'left 0 0
                                           (list (make-mc 3 3 'left 0 0 '()))))
              #true)
(check-expect (already-traversed? (make-mc 2 2 'right 1 1
                                           (list (make-mc 3 3 'left 0 0 '())
                                                 (make-mc 2 2 'right 1 1 (list (make-mc 3 3 'left 0 0 '()))))))
              #true)

(define (already-traversed? s0)
  (ormap (lambda (s) (mc=? s s0))
         (mc-path s0)))

; PuzzleState -> Boolean
; return #true if the state is valid
(check-expect (valid? S1) #true)
(check-expect (valid? S2) #true)
(check-expect (valid? S4) #true)
(check-expect (valid? S3) #true)

; number is negative
(check-expect (valid? (make-mc -1 2 'right 4 1 '())) #false)
(check-expect (valid? (make-mc 3 -1 'right 0 4 '())) #false)
(check-expect (valid? (make-mc 4 1 'left -1 2 '())) #false)
(check-expect (valid? (make-mc 3 4 'left 0 -1 '())) #false)

; cannibals > missionaries
(check-expect (valid? (make-mc 2 3 'right 1 0 '())) #false)
(check-expect (valid? (make-mc 1 0 'right 2 3 '())) #false)

; already traversed state
(check-expect (valid? (make-mc 3 3 'left 0 0
                               (list (make-mc 3 3 'left 0 0 '()))))
              #false)
(check-expect (valid? (make-mc 2 2 'right 1 1
                               (list (make-mc 3 3 'left 0 0 '())
                                     (make-mc 2 2 'right 1 1 (list (make-mc 3 3 'left 0 0 '()))))))
              #false)

(define (valid? s)
  (local ((define lm (mc-lm s))
          (define lc (mc-lc s))
          (define rm (mc-rm s))
          (define rc (mc-rc s)))
    (and (>= lm 0) (>= lc 0) (>= rm 0) (>= rc 0)
         (or (>= lm lc) (zero? lm)) (or (>= rm rc) (zero? rm))
         (not (already-traversed? s)))))

; PuzzleState -> [List-of PuzzleState]
; generates the list of all those states that a boat ride can reach from the given state
(check-expect (create-next-states/one S1) (list S2 S3 S4))
(check-expect (create-next-states/one S2) '())
(check-expect (create-next-states/one S4) (list S6))
(check-expect (create-next-states/one S6) (list S7 S8))

(define (create-next-states/one s)
  (local
    (; Boat
     (define boat (mc-boat s))
     ; [List-of PuzzleState]
     (define new-path (cons s (mc-path s)))
     ; [List-of PuzzleState]
     (define all-states
       (cond
         [(symbol=? boat 'left)
          (list (make-mc (- (mc-lm s) 1) (mc-lc s) 'right (+ (mc-rm s) 1) (mc-rc s) new-path)
                (make-mc (mc-lm s) (- (mc-lc s) 1) 'right (mc-rm s) (+ (mc-rc s) 1) new-path)
                (make-mc (- (mc-lm s) 1) (- (mc-lc s) 1) 'right (+ (mc-rm s) 1) (+ (mc-rc s) 1) new-path)
                (make-mc (- (mc-lm s) 2) (mc-lc s) 'right (+ (mc-rm s) 2) (mc-rc s) new-path)
                (make-mc (mc-lm s) (- (mc-lc s) 2) 'right (mc-rm s) (+ (mc-rc s) 2) new-path))]
         [(symbol=? boat 'right)
          (list (make-mc (+ (mc-lm s) 1) (mc-lc s) 'left (- (mc-rm s) 1) (mc-rc s) new-path)
                (make-mc (mc-lm s) (+ (mc-lc s) 1) 'left (mc-rm s) (- (mc-rc s) 1) new-path)
                (make-mc (+ (mc-lm s) 1) (+ (mc-lc s) 1) 'left (- (mc-rm s) 1) (- (mc-rc s) 1) new-path)
                (make-mc (+ (mc-lm s) 2) (mc-lc s) 'left (- (mc-rm s) 2) (mc-rc s) new-path)
                (make-mc (mc-lm s) (+ (mc-lc s) 2) 'left (mc-rm s) (- (mc-rc s) 2) new-path))]))
     ; [List-of PuzzleState]
     (define valid-states (filter valid? all-states)))
    valid-states))

; [List-of PuzzleState] -> [List-of PuzzleState]
; generates the list of all those states that a boat ride can reach
(check-expect (create-next-states (list S1)) (list S2 S3 S4))
(check-expect (create-next-states (list S2 S3 S4)) (list S5 S6))

(define (create-next-states los0)
  (foldl (lambda (s los)
           (append los (create-next-states/one s)))
         '()
         los0))

; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative: creates a tree of possible boat rides
(check-satisfied (solve S1) final?)

(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))

(define solution (solve S1))
(define path-to-solution (reverse (cons solution (mc-path solution))))
(define images (map render-mc path-to-solution))
(run-movie 1 images)
