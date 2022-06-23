;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |294|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurence of x in l, #false otherwise
(check-expect (index 3 '()) #false)
(check-expect (index 1 '(1 3 5)) 0)
(check-expect (index 3 '(1 3 5)) 1)
(check-expect (index 5 '(1 3 5)) 2)
(check-expect (index 6 '(1 3 5)) #false)
(check-expect (index "foo" '("hello" "world" "foo" "bar")) 2)
(check-expect (index 3 '(1 3 3 5 3)) 1)

(check-satisfied (index 3 '())
                 (is-index? 3 '()))
(check-satisfied (index 1 '(1 3 5))
                 (is-index? 1 '(1 3 5)))
(check-satisfied (index 3 '(1 3 5))
                 (is-index? 3 '(1 3 5)))
(check-satisfied (index 5 '(1 3 5))
                 (is-index? 5 '(1 3 5)))
(check-satisfied (index 6 '(1 3 5))
                 (is-index? 6 '(1 3 5)))
(check-satisfied (index "foo" '("hello" "world" "foo" "bar"))
                 (is-index? "foo" '("hello" "world" "foo" "bar")))
(check-satisfied (index 3 '(1 3 3 5 3))
                 (is-index? 3 '(1 3 3 5 3)))

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; [X] X [List-of X] -> [[Maybe N] -> Boolean]
; check that the result of "index" is correct
(check-expect [(is-index? 3 '()) #false] #true)
(check-expect [(is-index? 1 '(1 3 5)) 0] #true)
(check-expect [(is-index? 3 '(1 3 5)) 1] #true)
(check-expect [(is-index? 5 '(1 3 5)) 2] #true)
(check-expect [(is-index? 6 '(1 3 5)) #false] #true)
(check-expect [(is-index? "foo" '("hello" "world" "foo" "bar")) 2] #true)

(check-expect [(is-index? 3 '()) 0] #false)
(check-expect [(is-index? 5 '(1 3 5)) 1] #false)
(check-expect [(is-index? 5 '(1 3 5)) #false] #false)
(check-expect [(is-index? 5 '(1)) 9] #false)

(check-expect [(is-index? 3 '(1 3 3 5 3)) 1] #true)
(check-expect [(is-index? 3 '(1 3 3 5 3)) 2] #false)
(check-expect [(is-index? 3 '(1 3 3 5 3)) 4] #false)

(define (is-index? x l0)
  ; [Maybe N] -> Boolean
  (lambda (result)
    (if (false? result)
        (not (member? x l0))
        (and (< result (length l0))
             (equal? (list-ref l0 result) x)
             (if (zero? result)
                 #true
                 (local (; N -> Boolean
                         (define (prior-occurence? i)
                           (if (equal? (list-ref l0 i) x)
                               #true
                               (if (zero? i)
                                   #false
                                   (prior-occurence? (sub1 i))))))
                   (not (prior-occurence? (sub1 result)))))))))
