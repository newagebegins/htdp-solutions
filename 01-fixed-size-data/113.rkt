;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |113|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; Any -> Boolean
; is a an elements of the SIGS collection
(check-expect (sigs? (make-aim (make-posn 1 2) (make-tank 3 4))) #true)
(check-expect (sigs? (make-fired (make-posn 1 2) (make-tank 3 4) (make-posn 5 6))) #true)
(check-expect (sigs? #false) #false)
(check-expect (sigs? "hello") #false)

(define (sigs? a)
  (or (aim? a) (fired? a)))

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; Any -> Boolean
; is a an element of the Coordinate collection
(check-expect (coordinate? -1.3) #true)
(check-expect (coordinate? 4) #true)
(check-expect (coordinate? 0) #true)
(check-expect (coordinate? (make-posn 2.3 -5)) #true)
(check-expect (coordinate? "foo") #false)

(define (coordinate? a)
  (or (number? a) (posn? a)))

; XCoord is Natural[0, WIDTH]
; interp. x coordinate of the animal

; Happiness is a Natural[0, 100]
; interp. happiness level of the animal

(define-struct cat [x hap])
; VCat is (make-cat XCoord Happiness)
; interp. a cat at distance x from the left edge of the scene, with given happiness level

; Color is one of:
; - "red"
; - "green"
; - "blue"
; interp. color of the chameleon

(define-struct cham [x hap col])
; Cham is (make-cham XCoord Happiness Color)
; interp. a chameleon at distance x from the left edge of the scene, with given happiness level and color

; A VAnimal is either
; – a VCat
; – a VCham

; Any -> Boolean
; is a an element of the VAnimal collection
(check-expect (animal? (make-cat 3 99)) #true)
(check-expect (animal? (make-cham 3 99 "red")) #true)
(check-expect (animal? 3) #false)
(check-expect (animal? "a") #false)

(define (animal? a)
  (or (cat? a) (cham? a)))
