;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 276-select-albums) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; An LLTracks is one of:
; - '()
; - (cons LTracks LLTracks)
; interp. a list of lists of tracks

(define D1 (create-date 2000 1 1 1 1 1))

(define TA1 (create-track "TA1" "Art1" "AlbA" 1 1 D1 1 D1))
(define TA2 (create-track "TA2" "Art1" "AlbA" 1 1 D1 1 D1))
(define TA3 (create-track "TA3" "Art1" "AlbA" 1 1 D1 1 D1))

(define TB1 (create-track "TB1" "Art1" "AlbB" 1 1 D1 1 D1))
(define TB2 (create-track "TB2" "Art1" "AlbB" 1 1 D1 1 D1))

(define TC1 (create-track "TC1" "Art1" "AlbC" 1 1 D1 1 D1))

; LTracks -> List-of-strings
; produce the list of album titles (with duplicates)
(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles (list TA1)) '("AlbA"))
(check-expect (select-all-album-titles (list TA1 TA2)) '("AlbA" "AlbA"))
(check-expect (select-all-album-titles (list TB1)) '("AlbB"))
(check-expect (select-all-album-titles (list TA1 TA2 TB1 TB2 TC1)) '("AlbA" "AlbA" "AlbB" "AlbB" "AlbC"))

(define (select-all-album-titles lt)
  (map track-album lt))

; String List-of-strings -> List-of-strings
; adds str to set if it is not already in the set
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
  (foldr add-to-set '() ls))

; LTracks -> List-of-strings
; produce a list of unique album titles
(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))

; String LTracks -> LTracks
; extract from lt tracks that belong to the album a
(define (select-album a lt)
  (local (; Track -> Boolean
          (define (belongs-to-album? t)
            (string=? (track-album t) a)))
    (filter belongs-to-album? lt)))

; List-of-strings LTracks -> LLTracks
(check-expect (group-by-album '() '()) '())
(check-expect (group-by-album (list "AlbA") (list TA1)) (list (list TA1)))
(check-expect (group-by-album (list "AlbA" "AlbB") (list TA1 TB1)) (list (list TA1) (list TB1)))
(check-expect (group-by-album (list "AlbA" "AlbB" "AlbC") (list TA1 TA2 TB1 TA3 TB2 TC1))
              (list (list TA1 TA2 TA3)
                    (list TB1 TB2)
                    (list TC1)))

(define (group-by-album la lt)
  (local (; String -> LTracks
          (define (album-tracks a)
            (select-album a lt)))
    (map album-tracks la)))

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
