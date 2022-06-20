;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |208|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "../assets/itunes.xml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define T0 (list (list "str" "hello")))
(define T1 (list (list "b1" #true)))
(define T2 (list (list "b2" #false)))
(define T3 (list (list "b1" #false) (list "b2" #true)))
(define T4 (list (list "str" "hello") (list "bb" #false) (list "nn" 123)))

; LAssoc -> List-of-strings
; produce keys that are associated with boolean attributes for the given track
(check-expect (boolean-attributes1 '()) '())
(check-expect (boolean-attributes1 T0) '())
(check-expect (boolean-attributes1 T1) (list "b1"))
(check-expect (boolean-attributes1 T2) (list "b2"))
(check-expect (boolean-attributes1 T3) (list "b1" "b2"))
(check-expect (boolean-attributes1 T4) (list "bb"))

(define (boolean-attributes1 la)
  (cond
    [(empty? la) '()]
    [else (if (boolean? (second (first la)))
              (cons (first (first la)) (boolean-attributes1 (rest la)))
              (boolean-attributes1 (rest la)))]))

; LLists -> List-of-strings
; produce keys that are associated with boolean attributes for all tracks in the list (with duplicates)
(check-expect (boolean-attributes-all '()) '())
(check-expect (boolean-attributes-all (list T0)) '())
(check-expect (boolean-attributes-all (list T1)) (list "b1"))
(check-expect (boolean-attributes-all (list T2)) (list "b2"))
(check-expect (boolean-attributes-all (list T3)) (list "b1" "b2"))
(check-expect (boolean-attributes-all (list T4)) (list "bb"))
(check-expect (boolean-attributes-all (list T0 T1 T2 T3 T4)) (list "b1" "b2" "b1" "b2" "bb"))

(define (boolean-attributes-all ll)
  (cond
    [(empty? ll) '()]
    [else (append (boolean-attributes1 (first ll)) (boolean-attributes-all (rest ll)))]))

; LLists -> List-of-strings
; produce keys that are associated with boolean attributes for all tracks in the list
(check-expect (boolean-attributes '()) '())
(check-expect (boolean-attributes (list T0)) '())
(check-expect (boolean-attributes (list T1)) (list "b1"))
(check-expect (boolean-attributes (list T2)) (list "b2"))
(check-expect (boolean-attributes (list T3)) (list "b1" "b2"))
(check-expect (boolean-attributes (list T4)) (list "bb"))
(check-expect (boolean-attributes (list T0 T1 T2 T3 T4)) (list "b1" "b2" "bb"))

(define (boolean-attributes ll)
  (create-set (boolean-attributes-all ll)))

; (define list-tracks (read-itunes-as-lists ITUNES-LOCATION))

; > (boolean-attributes list-tracks)
; (list "Disabled" "Compilation" "Purchased")
