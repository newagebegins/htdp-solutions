;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |217|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; logical dimensions of the playing field
(define COLUMNS 15)
(define ROWS 10)

; size of one cell in pixels
(define CELL-SIZE 20)

; pixel dimensions of the playing field
(define WIDTH (* COLUMNS CELL-SIZE))
(define HEIGHT (* ROWS CELL-SIZE))

(define SEG-R (/ CELL-SIZE 2))                       ; segment radius
(define SEG (circle (/ CELL-SIZE 2) "solid" "red"))  ; segment
(define MTS (empty-scene WIDTH HEIGHT))              ; empty scene

; A Segment is a Posn:
;   (make-posn Integer Integer)
; interp. represents a position of a worm segment on the playfield

; A List-of-segments is one of:
; - '()
; - (cons Segment List-of-segments)
; interp. a list of worm segments, the first segment represents the head

; A Direction is one of:
; - "right"
; - "left"
; - "up"
; - "down"
; interp. the direction of movement of the worm's head

(define-struct worm [seg dir])
; A Worm is a structure:
;   (make-worm List-of-segments Direction)
; interp. the worm consisting of segments, it's head moving in the given direction

; Segment Image -> Image
; render a segment onto the given image
(check-expect (render-segment (make-posn 2 3) MTS)
              (place-image SEG
                           (+ SEG-R (* 2 CELL-SIZE))
                           (+ SEG-R (* 3 CELL-SIZE))
                           MTS))

(define (render-segment s img)
  (place-image SEG
               (+ SEG-R (* (posn-x s) CELL-SIZE))
               (+ SEG-R (* (posn-y s) CELL-SIZE))
               img))

; List-of-segments Image -> Image
; render segments onto the given image
(check-expect (render-segments '() MTS) MTS)
(check-expect (render-segments (list (make-posn 0 0)) MTS)
              (place-image SEG SEG-R SEG-R MTS))
(check-expect (render-segments (list (make-posn 0 0)
                                     (make-posn 1 0)
                                     (make-posn 1 1)
                                     (make-posn 2 1)) MTS)
              (place-image SEG SEG-R SEG-R
                           (place-image SEG (+ SEG-R CELL-SIZE) SEG-R
                                        (place-image SEG (+ SEG-R CELL-SIZE) (+ SEG-R CELL-SIZE)
                                                     (place-image SEG (+ SEG-R (* 2 CELL-SIZE)) (+ SEG-R CELL-SIZE)
                                                                  MTS)))))

(define (render-segments los img)
  (cond
    [(empty? los) img]
    [else (render-segment (first los) (render-segments (rest los) img))]))

; Worm -> Image
; render the worm
(check-expect (render-worm (make-worm (list (make-posn 0 0)
                                            (make-posn 1 0)
                                            (make-posn 1 1)
                                            (make-posn 2 1)) "up"))
              (place-image SEG SEG-R SEG-R
                           (place-image SEG (+ SEG-R CELL-SIZE) SEG-R
                                        (place-image SEG (+ SEG-R CELL-SIZE) (+ SEG-R CELL-SIZE)
                                                     (place-image SEG (+ SEG-R (* 2 CELL-SIZE)) (+ SEG-R CELL-SIZE)
                                                                  MTS)))))
(define (render-worm w)
  (render-segments (worm-seg w) MTS))

; Segment Direction -> Segment
; move the head segment to the new position in the given direction
(check-expect (move-head (make-posn 0 3) "right") (make-posn 1 3))
(check-expect (move-head (make-posn 0 3) "left") (make-posn -1 3))
(check-expect (move-head (make-posn 0 3) "up") (make-posn 0 2))
(check-expect (move-head (make-posn 0 3) "down") (make-posn 0 4))

(define (move-head head dir)
  (cond
    [(string=? dir "right") (make-posn (add1 (posn-x head)) (posn-y head))]
    [(string=? dir "left") (make-posn (sub1 (posn-x head)) (posn-y head))]
    [(string=? dir "up") (make-posn (posn-x head) (sub1 (posn-y head)))]
    [(string=? dir "down") (make-posn (posn-x head) (add1 (posn-y head)))]))

; List -> List
; remove the last element of the list
(check-expect (remove-last '()) '())
(check-expect (remove-last (list 1)) '())
(check-expect (remove-last (list 1 2)) (list 1))
(check-expect (remove-last (list 1 2 3)) (list 1 2))

(define (remove-last l)
  (cond
    [(<= (length l) 1) '()]
    [else (cons (first l) (remove-last (rest l)))]))

; Worm -> Worm
; update worm state for one tick
(check-expect (update-worm (make-worm (list (make-posn 5 3) (make-posn 4 3) (make-posn 4 2)) "right"))
              (make-worm (list (make-posn 6 3) (make-posn 5 3) (make-posn 4 3)) "right"))
(check-expect (update-worm (make-worm (list (make-posn 5 5) (make-posn 5 4) (make-posn 6 4)) "left"))
              (make-worm (list (make-posn 4 5) (make-posn 5 5) (make-posn 5 4)) "left"))

(define (update-worm w)
  (make-worm (cons (move-head (first (worm-seg w)) (worm-dir w))
                   (remove-last (worm-seg w)))
             (worm-dir w)))

; Worm KeyEvent -> Worm
; handle key events
(check-expect (handle-keys (make-worm '() "up") "right") (make-worm '() "right"))
(check-expect (handle-keys (make-worm '() "up") "left") (make-worm '() "left"))
(check-expect (handle-keys (make-worm '() "left") "up") (make-worm '() "up"))
(check-expect (handle-keys (make-worm '() "left") "down") (make-worm '() "down"))
(check-expect (handle-keys (make-worm '() "left") "a") (make-worm '() "left"))

(define (handle-keys w k)
  (cond
    [(key=? k "left") (make-worm (worm-seg w) "left")]
    [(key=? k "right") (make-worm (worm-seg w) "right")]
    [(key=? k "up") (make-worm (worm-seg w) "up")]
    [(key=? k "down") (make-worm (worm-seg w) "down")]
    [else w]))

; Number -> Worm
; run the program with the given clock rate
(define (worm-main rate)
  (big-bang (make-worm (list (make-posn 5 5)
                             (make-posn 6 5)
                             (make-posn 6 6)
                             (make-posn 7 6)
                             (make-posn 7 7)
                             (make-posn 8 7)
                             (make-posn 9 7)) "right")
    [to-draw render-worm]
    [on-tick update-worm rate]
    [on-key handle-keys]))
