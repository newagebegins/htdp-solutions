;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |204|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; LTracks -> List-of-strings
; produce the list of album titles (with duplicates)
(define (select-all-album-titles lt)
  (cond
    [(empty? lt) '()]
    [else (cons (track-album (first lt)) (select-all-album-titles (rest lt)))]))

; String List-of-strings -> List-of-strings
; adds str to set if it is not already in the set
(define (add-to-set str set)
  (if (member? str set)
      set
      (cons str set)))

; List-of-strings -> List-of-strings
; removes duplicate strings from the list
(define (create-set ls)
  (cond
    [(empty? ls) '()]
    [else (add-to-set (first ls) (create-set (rest ls)))]))

; LTracks -> List-of-strings
; produce a list of unique album titles
(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))

; String LTracks -> LTracks
; extract from lt tracks that belong to the album a
(define (select-album a lt)
  (cond
    [(empty? lt) '()]
    [else (if (string=? (track-album (first lt)) a)
              (cons (first lt) (select-album a (rest lt)))
              (select-album a (rest lt)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ITUNES-LOCATION "../assets/itunes.xml")

(define D1 (create-date 2000 1 1 1 1 1))

(define TA1 (create-track "TA1" "Art1" "AlbA" 1 1 D1 1 D1))
(define TA2 (create-track "TA2" "Art1" "AlbA" 1 1 D1 1 D1))
(define TA3 (create-track "TA3" "Art1" "AlbA" 1 1 D1 1 D1))

(define TB1 (create-track "TB1" "Art1" "AlbB" 1 1 D1 1 D1))
(define TB2 (create-track "TB2" "Art1" "AlbB" 1 1 D1 1 D1))

(define TC1 (create-track "TC1" "Art1" "AlbC" 1 1 D1 1 D1))

; An LLTracks is one of:
; - '()
; - (cons LTracks LLTracks)
; interp. a list of lists of tracks

; List-of-strings LTracks -> LLTracks
(check-expect (group-by-album '() '()) '())
(check-expect (group-by-album (list "AlbA") (list TA1)) (list (list TA1)))
(check-expect (group-by-album (list "AlbA" "AlbB") (list TA1 TB1)) (list (list TA1) (list TB1)))
(check-expect (group-by-album (list "AlbA" "AlbB" "AlbC") (list TA1 TA2 TB1 TA3 TB2 TC1))
              (list (list TA1 TA2 TA3)
                    (list TB1 TB2)
                    (list TC1)))

(define (group-by-album la lt)
  (cond
    [(empty? la) '()]
    [else (cons (select-album (first la) lt) (group-by-album (rest la) lt))]))

; LTracks -> LLTracks
; group tracks by album
(check-expect (select-albums '()) '())
(check-expect (select-albums (list TA1)) (list (list TA1)))
(check-expect (select-albums (list TA1 TB1)) (list (list TA1) (list TB1)))
(check-expect (select-albums (list TA1 TA2 TB1 TA3 TB2 TC1))
              (list (list TA1 TA2 TA3)
                    (list TB1 TB2)
                    (list TC1)))

(define (select-albums lt)
  (group-by-album (select-album-titles/unique lt) lt))
