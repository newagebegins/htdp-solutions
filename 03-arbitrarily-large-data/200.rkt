;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |200|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "../assets/itunes.xml")

(define D1 (create-date 1999 6 25 21 50 49))
(define D2 (create-date 2005 6 25 21 50 0))

(define T1 (create-track "Track #1" "Artist #1" "Album #1" 100000 3 D1 10 D2))
(define T2 (create-track "Track #2" "Artist #2" "Album #2" 200000 4 D1 10 D2))
(define T3 (create-track "Track #3" "Artist #3" "Album #3" 300000 4 D1 10 D2))

(define LT1 (list T1))
(define LT2 (list T1 T2 T3))

; LTracks -> N
; given the list of tracks produce the total amount of play time
(check-expect (total-time '()) 0)
(check-expect (total-time LT1) 100000)
(check-expect (total-time LT2) 600000)

(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [else (+ (track-time (first lt)) (total-time (rest lt)))]))

; (define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))

; > (total-time itunes-tracks)
; 467897977
