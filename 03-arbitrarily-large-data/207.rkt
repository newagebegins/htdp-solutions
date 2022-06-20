;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |207|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; String LAssoc Any -> Any
; produces the first association whose first item is equal to key, or default if there is no such association
(define (find-association key la default)
  (cond
    [(empty? la) default]
    [else (if (string=? key (first (first la)))
              (first la)
              (find-association key (rest la) default))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ITUNES-LOCATION "../assets/itunes.xml")
(define TIME-KEY "Total Time") ;(list "Total Time" 188813)
 
;(define list-tracks (read-itunes-as-lists ITUNES-LOCATION))

(define T0 (list (list "k1" "hello")))
(define T1 (list (list TIME-KEY 1)))
(define T2 (list (list "k1" "hello")
                 (list TIME-KEY 2)))
(define T3 (list (list "k1" "hello")
                 (list TIME-KEY 3)
                 (list "k2" #true)))

; LAssoc -> N
; extract the play time of the track
(check-expect (total-time1 '()) 0)
(check-expect (total-time1 T0) 0)
(check-expect (total-time1 T1) 1)
(check-expect (total-time1 T2) 2)
(check-expect (total-time1 T3) 3)

(define (total-time1 la)
  (second (find-association TIME-KEY la (list TIME-KEY 0))))

; LLists -> N
; produces the total amount of play time
(check-expect (total-time/list '()) 0)
(check-expect (total-time/list (list T0)) 0)
(check-expect (total-time/list (list T1)) 1)
(check-expect (total-time/list (list T2)) 2)
(check-expect (total-time/list (list T3)) 3)
(check-expect (total-time/list (list T0 T1 T2 T3)) 6)

(define (total-time/list ll)
  (cond
    [(empty? ll) 0]
    [else (+ (total-time1 (first ll)) (total-time/list (rest ll)))]))

; > (total-time/list list-tracks)
; 494030470
