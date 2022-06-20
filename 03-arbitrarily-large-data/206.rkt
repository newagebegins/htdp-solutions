;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |206|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String LAssoc Any -> Any
; produces the first association whose first item is equal to key, or default if there is no such association
(check-expect (find-association "k1" '() 100) 100)
(check-expect (find-association "k1" '() #false) #false)
(check-expect (find-association "k1" (list (list "k1" "hello")) 1)
              (list "k1" "hello"))
(check-expect (find-association "k1"
                                (list (list "k1" "hello")
                                      (list "k1" "world"))
                                1)
              (list "k1" "hello"))
(check-expect (find-association "k2" (list (list "k1" "hello")) 1) 1)
(check-expect (find-association "k2"
                                (list (list "k1" "hello")
                                      (list "k2" "world"))
                                1)
              (list "k2" "world"))
(check-expect (find-association "k2"
                                (list (list "k1" "hello")
                                      (list "k2" #true)
                                      (list "k3" "world"))
                                1)
              (list "k2" #true))

(define (find-association key la default)
  (cond
    [(empty? la) default]
    [else (if (string=? key (first (first la)))
              (first la)
              (find-association key (rest la) default))]))
