;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |103|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct spider [legs vol])
; A Spider is a structure:
;   (make-spider Natural PositiveNumber)
; interp. (make-spider l v) is a spider with l remaining legs,
; it needs space v for transportation

(define-struct boa-constrictor [length girth])
; A BoaConstrictor is a structure:
;   (make-boa-constrictor PositiveNumber PositiveNumber)
; interp. (make-boa-constrictor l g) is a boa constrictor with length l and girth g

(define-struct armadillo [age vol])
; An Armadillo is a structure:
;   (make-armadillo PositiveNumber PositiveNumber)
; interp. (make-armadillo a v) is an armadillo with age a and volume v

; An Elephant is a PositiveNumber
; interp. space needed for transportation of an elephant

; A ZooAnimal is one of:
; - Spider
; - BoaConstrictor
; - Armadillo
; - Elephant
; interp. a zoo animal that needs space for transportation

(define (fn-for-zoo-animal a)
  (cond
    [(spider? a) (... (spider-legs a) ... (spider-vol a) ...)]
    [(boa-constrictor? a) (... (boa-constrictor-length a) ... (boa-constrictor-girth a) ...)]
    [(armadillo? a) (... (armadillo-age a) ... (armadillo-vol a) ...)]
    [else ...]))

; ZooAnimal PositiveNumber -> Boolean
; determine whether an animal a fits inside a cage of volume v
(check-expect (fits? (make-spider 8 5) 6) #true)
(check-expect (fits? (make-spider 8 5) 5) #true)
(check-expect (fits? (make-spider 8 5) 4) #false)

(check-expect (fits? (make-boa-constrictor 3.3 2) 7) #true)
(check-expect (fits? (make-boa-constrictor 3.3 2) 6.6) #true)
(check-expect (fits? (make-boa-constrictor 3.3 2) 6) #false)

(check-expect (fits? (make-armadillo 5 3) 4) #true)
(check-expect (fits? (make-armadillo 5 3) 3) #true)
(check-expect (fits? (make-armadillo 5 3) 2) #false)

(check-expect (fits? 5 6) #true)
(check-expect (fits? 5 5) #true)
(check-expect (fits? 5 4) #false)

(define (fits? a v)
  (cond
    [(spider? a) (<= (spider-vol a) v)]
    [(boa-constrictor? a) (<= (* (boa-constrictor-length a) (boa-constrictor-girth a)) v)]
    [(armadillo? a) (<= (armadillo-vol a) v)]
    [else (<= a v)]))
