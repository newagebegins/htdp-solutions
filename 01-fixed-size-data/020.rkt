;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |020|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ i 1))))

(string-delete "123" 0)
(string-delete "123" 1)
(string-delete "123" 2)
