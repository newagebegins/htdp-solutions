;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |442|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (sorted<? l)
  (cond
    [(empty? (rest l)) #true]
    [(cons? (rest l)) (and (<= (first l) (first (rest l)))
                           (sorted<? (rest l)))]))

; Number [List-of Number] -> [List-of Number]
; inserts n into the sorted list of numbers alon
(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else (if (> n (first alon))
              (cons (first alon) (insert n (rest alon)))
              (cons n alon))]))

; [List-of Number] -> [List-of Number]
; rearranges alon in ascending order
(define (sort< alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon) (sort< (rest alon)))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers (rest alon) pivot))
                    (list pivot)
                    (quick-sort< (largers (rest alon) pivot))))]))

; [List-of Number] Number -> [List-of Number]
; produce a list of all the numbers in alon that are strictly larger than n
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
; produce a list of all the numbers in alon that are strictly smaller than n
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (<= (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

; N -> [List-of Number]
; creates a list of n random numbers
(check-expect (random-numbers 0) '())
(check-expect (length (random-numbers 10)) 10)

(define (random-numbers n)
  (local (; N -> [List-of Number]
          (define (create-list x)
            (cond
              [(zero? x) '()]
              [else (cons (random n) (create-list (sub1 x)))])))
    (create-list n)))

(define N1 (random-numbers 100))

(define S (time (sort< N1)))
(define QS (time (quick-sort< N1)))

; my measures with time function show that quick-sort is always faster
; on sets smaller than 100 both functions show 0 time
; this cross-over point is chosen arbitrarily for the sake of completing the exercise
(define CROSS-OVER 100)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(check-satisfied (clever-sort (random-numbers CROSS-OVER)) sorted<?)
(check-satisfied (clever-sort (random-numbers (add1 CROSS-OVER))) sorted<?)

(define (clever-sort alon)
  (if (<= (length alon) CROSS-OVER)
      (sort< alon)
      (quick-sort< alon)))
