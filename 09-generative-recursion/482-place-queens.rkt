;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 482-place-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(zero? n) a-board]
    [else
     (local (; [List-of QP] -> [Maybe [List-of QP]]
             (define (try-place spots)
               (cond
                 [(empty? spots) #false]
                 [else
                  (local (; [Maybe [List-of QP]]
                          (define qps (place-queens (add-queen a-board (first spots))
                                                    (sub1 n))))
                    (if (false? qps)
                        (try-place (rest spots))
                        qps))])))
       (try-place (find-open-spots a-board)))]))

; N -> Board
; creates the initial n by n board
(define (board0 n) ...)

; Board QP -> Board
; places a queen at qp on a-board
(define (add-queen a-board qp)
  a-board)

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  '())
