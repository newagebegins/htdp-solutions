;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |202|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "../assets/itunes.xml")

(define D1 (create-date 1999 6 25 21 50 49))
(define D2 (create-date 2005 6 25 21 50 0))

(define T1 (create-track "Track #1" "Artist #1" "Album #1" 100000 3 D1 10 D2))
(define T2 (create-track "Track #2" "Artist #2" "Album #2" 200000 4 D1 10 D2))
(define T3 (create-track "Track #3" "Artist #3" "Album #3" 300000 4 D1 10 D2))
(define T4 (create-track "Track #4" "Artist #2" "Album #2" 300000 4 D1 10 D2))

(define LT1 (list T1))
(define LT2 (list T1 T2 T3))
(define LT3 (list T1 T2 T3 T4))

; String LTracks -> LTracks
; extract from lt tracks that belong to the album a
(check-expect (select-album "Album #2" '()) '())
(check-expect (select-album "Album #2" LT3) (list T2 T4))
(check-expect (select-album "Album #1" LT3) (list T1))
(check-expect (select-album "Album #3" LT3) (list T3))
(check-expect (select-album "foo" LT3) '())

(define (select-album a lt)
  (cond
    [(empty? lt) '()]
    [else (if (string=? (track-album (first lt)) a)
              (cons (first lt) (select-album a (rest lt)))
              (select-album a (rest lt)))]))

;(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))
