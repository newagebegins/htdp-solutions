;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |203|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "../assets/itunes.xml")

(define D1 (create-date 1998 1 1 1 1 1))
(define D2 (create-date 1998 1 1 1 1 2))
(define D3 (create-date 1998 1 1 1 2 1))
(define D4 (create-date 1998 1 1 2 1 1))
(define D5 (create-date 1998 1 2 1 1 1))
(define D6 (create-date 1998 2 1 1 1 1))
(define D7 (create-date 1999 1 1 1 1 1))
(define D8 (create-date 2000 1 1 1 1 1))

(define T1 (create-track "Track #1" "Artist #1" "Album #1" 100 3 D1 10 D1))
(define T2 (create-track "Track #2" "Artist #1" "Album #1" 100 3 D1 10 D2))
(define T3 (create-track "Track #3" "Artist #1" "Album #1" 100 3 D1 10 D3))
(define T4 (create-track "Track #4" "Artist #1" "Album #1" 100 3 D1 10 D4))
(define T5 (create-track "Track #5" "Artist #1" "Album #1" 100 3 D1 10 D5))
(define T6 (create-track "Track #6" "Artist #1" "Album #1" 100 3 D1 10 D6))
(define T7 (create-track "Track #7" "Artist #1" "Album #1" 100 3 D1 10 D7))

(define T8 (create-track "Foo" "Artist #2" "Album #2" 100 3 D1 10 D7))

(define LT1 (list T1 T2 T3 T4 T5 T6 T7 T8))
(define LT2 (list T1 T2 T3 T4 T8 T5 T6 T7))

; Date Date -> Boolean
; return #true if d1 is strictly greater than d2
(check-expect (date>=? D1 D1) #true)
(check-expect (date>=? D2 D1) #true)
(check-expect (date>=? D3 D1) #true)
(check-expect (date>=? D4 D1) #true)
(check-expect (date>=? D5 D1) #true)
(check-expect (date>=? D6 D1) #true)
(check-expect (date>=? D7 D1) #true)

(check-expect (date>=? D3 D2) #true)
(check-expect (date>=? D7 D5) #true)

(check-expect (date>=? D1 D2) #false)
(check-expect (date>=? D1 D2) #false)
(check-expect (date>=? D1 D2) #false)
(check-expect (date>=? D1 D2) #false)
(check-expect (date>=? D1 D2) #false)

(check-expect (date>=? D1 D5) #false)
(check-expect (date>=? D3 D6) #false)

(define (date>=? d1 d2)
  (cond [(> (date-year d1) (date-year d2)) #true]
        [(< (date-year d1) (date-year d2)) #false]
        [else (cond [(> (date-month d1) (date-month d2)) #true]
                    [(< (date-month d1) (date-month d2)) #false]
                    [else (cond [(> (date-day d1) (date-day d2)) #true]
                                [(< (date-day d1) (date-day d2)) #false]
                                [else (cond [(> (date-hour d1) (date-hour d2)) #true]
                                            [(< (date-hour d1) (date-hour d2)) #false]
                                            [else (cond [(> (date-minute d1) (date-minute d2)) #true]
                                                        [(< (date-minute d1) (date-minute d2)) #false]
                                                        [else (cond [(> (date-second d1) (date-second d2)) #true]
                                                                    [(< (date-second d1) (date-second d2)) #false]
                                                                    [else #true])])])])])]))

; String Date LTracks -> LTracks
; extract from lt tracks that belong to the album a and have been played after the date d
(check-expect (select-album-date "foo" D1 '()) '())
(check-expect (select-album-date "Album #1" D1 LT1) (list T1 T2 T3 T4 T5 T6 T7))
(check-expect (select-album-date "Album #1" D1 LT2) (list T1 T2 T3 T4 T5 T6 T7))
(check-expect (select-album-date "Album #2" D1 LT1) (list T8))
(check-expect (select-album-date "Album #1" D2 LT1) (list T2 T3 T4 T5 T6 T7))
(check-expect (select-album-date "Album #1" D3 LT1) (list T3 T4 T5 T6 T7))
(check-expect (select-album-date "Album #1" D4 LT1) (list T4 T5 T6 T7))
(check-expect (select-album-date "Album #1" D5 LT1) (list T5 T6 T7))
(check-expect (select-album-date "Album #1" D6 LT1) (list T6 T7))
(check-expect (select-album-date "Album #1" D7 LT1) (list T7))
(check-expect (select-album-date "Album #1" D8 LT1) '())

(define (select-album-date a d lt)
  (cond
    [(empty? lt) '()]
    [else (if (and (string=? (track-album (first lt)) a)
                   (date>=? (track-played (first lt)) d))
              (cons (first lt) (select-album-date a d (rest lt)))
              (select-album-date a d (rest lt)))]))

;(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))
