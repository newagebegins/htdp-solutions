;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |165|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; List-of-strings -> List-of-strings
; replaces all occurrences of "robot" with "r2d2" in the list; all other descriptions remain the same.
(check-expect (subst-robot '()) '())
(check-expect (subst-robot (cons "robot" '())) (cons "r2d2" '()))
(check-expect (subst-robot (cons "car" '())) (cons "car" '()))
(check-expect (subst-robot (cons "bear" (cons "robot" (cons "doll" '()))))
              (cons "bear" (cons "r2d2" (cons "doll" '()))))
(check-expect (subst-robot (cons "robot" (cons "robot" (cons "car" '()))))
              (cons "r2d2" (cons "r2d2" (cons "car" '()))))

(define (subst-robot l)
  (cond
    [(empty? l) '()]
    [else (cons (if (string=? "robot" (first l))
                    "r2d2"
                    (first l))
                (subst-robot (rest l)))]))

; String String List-of-strings -> List-of-strings
; replaces all occurrences of `old` with `new` in the list; all other descriptions remain the same.
(check-expect (substitute "robot" "r2d2" '()) '())
(check-expect (substitute "robot" "r2d2" (cons "robot" '())) (cons "r2d2" '()))
(check-expect (substitute "robot" "r2d2" (cons "car" '())) (cons "car" '()))
(check-expect (substitute "robot" "r2d2" (cons "bear" (cons "robot" (cons "doll" '()))))
              (cons "bear" (cons "r2d2" (cons "doll" '()))))
(check-expect (substitute "robot" "r2d2" (cons "robot" (cons "robot" (cons "car" '()))))
              (cons "r2d2" (cons "r2d2" (cons "car" '()))))

(define (substitute old new l)
    (cond
    [(empty? l) '()]
    [else (cons (if (string=? old (first l))
                    new
                    (first l))
                (substitute old new (rest l)))]))
