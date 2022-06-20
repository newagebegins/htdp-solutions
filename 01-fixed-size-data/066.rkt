;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |066|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
(define SAMPLE-MOVIE (make-movie "Star Wars" "Gary Kurtz" 1977))

(define-struct person [name hair eyes phone])
(define SAMPLE-PERSON (make-person "Bob" "black" "blue" "123-456"))

(define-struct pet [name number])
(define SAMPLE-PET (make-pet "Spike" "777"))

(define-struct CD [artist title price])
(define SAMPLE-CD (make-CD "Sum41" "Underclass Hero" 14.99))

(define-struct sweater [material size producer])
(define SAMPLE-SWEATER (make-sweater "wool" 36 "Columbia"))
