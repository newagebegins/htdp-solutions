;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |199|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define D1 (create-date 1999 6 25 21 50 49))
(define D2 (create-date 2005 6 25 21 50 0))

(define T1 (create-track "Some name" "Some artist" "Some album" 100000 3 D1 10 D2))
(define T2 (create-track "Some name#2" "Some artist#2" "Some album#2" 200000 4 D1 10 D2))

(define LT1 (list T1 T2))
