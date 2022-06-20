;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |201|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; LTracks -> List-of-strings
; produce the list of album titles (with duplicates)
(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles LT1) (list "Album #1"))
(check-expect (select-all-album-titles LT2) (list "Album #1" "Album #2" "Album #3"))
(check-expect (select-all-album-titles LT3) (list "Album #1" "Album #2" "Album #3" "Album #2"))

(define (select-all-album-titles lt)
  (cond
    [(empty? lt) '()]
    [else (cons (track-album (first lt)) (select-all-album-titles (rest lt)))]))

; String List-of-strings -> List-of-strings
; adds str to set if it is not already in the set
(check-expect (add-to-set "foo" '()) (list "foo"))
(check-expect (add-to-set "foo" (list "foo")) (list "foo"))
(check-expect (add-to-set "foo" (list "foo" "bar")) (list "foo" "bar"))
(check-expect (add-to-set "bar" (list "foo" "bar")) (list "foo" "bar"))
(check-expect (add-to-set "a" (list "b" "c")) (list "a" "b" "c"))

(define (add-to-set str set)
  (if (member? str set)
      set
      (cons str set)))

; List-of-strings -> List-of-strings
; removes duplicate strings from the list
(check-expect (create-set '()) '())
(check-expect (create-set (list "foo")) (list "foo"))
(check-expect (create-set (list "foo" "foo")) (list "foo"))
(check-expect (create-set (list "foo" "foo" "foo")) (list "foo"))
(check-expect (create-set (list "a" "b" "c")) (list "a" "b" "c"))
(check-expect (create-set (list "a" "b" "b" "c" "c" "a" "b" "c" "d" "a")) (list "b" "c" "d" "a"))
(check-expect (create-set (list "a" "b" "b" "c" "c" "b" "c" "d")) (list "a" "b" "c" "d"))

(define (create-set ls)
  (cond
    [(empty? ls) '()]
    [else (add-to-set (first ls) (create-set (rest ls)))]))

; LTracks -> List-of-strings
; produce a list of unique album titles
(check-expect (select-album-titles/unique '()) '())
(check-expect (select-album-titles/unique LT1) (list "Album #1"))
(check-expect (select-album-titles/unique LT2) (list "Album #1" "Album #2" "Album #3"))
(check-expect (select-album-titles/unique LT3) (list "Album #1" "Album #3" "Album #2"))

(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))

;(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))
;(select-album-titles/unique itunes-tracks)
