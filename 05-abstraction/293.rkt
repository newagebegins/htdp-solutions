;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |293|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts with x, #false otherwise
(check-expect (find 2 '(2 1 3)) '(2 1 3))
(check-expect (find 1 '(2 1 3)) '(1 3))
(check-expect (find 3 '(2 1 3)) '(3))
(check-expect (find 4 '(2 1 3)) #false)
(check-expect (find 1 '(2 1 3 2 1 3)) '(1 3 2 1 3))

(check-satisfied (find 1 '(2 1 3)) (found? 1 '(2 1 3)))

(define (find x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              l
              (find x (rest l)))]))

; [X] X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
; check that the result of "find" is correct
(check-expect [(found? 2 '(2 1 3)) '(2 1 3)] #true)
(check-expect [(found? 1 '(2 1 3)) '(1 3)] #true)
(check-expect [(found? 3 '(2 1 3)) '(3)] #true)
(check-expect [(found? 4 '(2 1 3)) #false] #true)

(check-expect [(found? 2 '(2 1 3)) #false] #false)
(check-expect [(found? 1 '(2 1 3)) '(3)] #false)
(check-expect [(found? 1 '(2 1 3)) '(2 1 3)] #false)
(check-expect [(found? 1 '(2 1 3 2 1 3)) '(1 3)] #false)
(check-expect [(found? 1 '(2 1 3 2 1 3)) '(1 3 2 1 3)] #true)
(check-expect [(found? 4 '(2 1 3)) '(1 3)] #false)

(define (found? x l0)
  ; [Maybe [List-of X]] -> Boolean
  (lambda (result)
    (if (false? result)
        (not (member? x l0))
        (local (; [List-of X] -> Boolean
                (define (first-sublist-starts-with-x? l)
                  (cond
                    [(empty? l) #false]
                    [else (if (equal? (first l) x)
                              (equal? l result)
                              (first-sublist-starts-with-x? (rest l)))])))
          (first-sublist-starts-with-x? l0)))))
