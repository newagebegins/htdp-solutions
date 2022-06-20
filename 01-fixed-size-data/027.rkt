;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |027|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define BASE_ATTENDEES 120)
(define BASE_TICKET_PRICE 5.0)
(define ATTENDEES_PER_DOLLAR (/ 15 0.1))
(define FIXED_COST 180)
(define COST_PER_ATTENDEE 0.04)

(define (attendees ticket-price)
  (- BASE_ATTENDEES (* (- ticket-price BASE_TICKET_PRICE) ATTENDEES_PER_DOLLAR)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FIXED_COST (* COST_PER_ATTENDEE (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
