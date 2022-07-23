;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 510-fmt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; N [List-of String] -> [List-of String]
; arrange the words into lines of maximal width w
(check-expect (fmt/l 15 (list "Lorem" "ipsum" "dolor" "sit" "amet," "consectetur"
                              "adipiscing" "elit." "Morbi" "quis" "vehicula" "lectus,"
                              "ut" "pulvinar" "est."))
              '("Lorem ipsum"
                "dolor sit amet,"
                "consectetur"
                "adipiscing"
                "elit. Morbi"
                "quis vehicula"
                "lectus, ut"
                "pulvinar est."))

(check-expect (fmt/l 20 (list "Lorem" "ipsum" "dolor" "sit" "amet," "consectetur"
                              "adipiscing" "elit." "Morbi" "quis" "vehicula" "lectus,"
                              "ut" "pulvinar" "est."))
              '("Lorem ipsum dolor"
                "sit amet,"
                "consectetur"
                "adipiscing elit."
                "Morbi quis vehicula"
                "lectus, ut pulvinar"
                "est."))

(define (fmt/l w words0)
  (local (; [List-of String] String [List-of String]
          ; accumulator: line - a line under construction
          ; accumulator: lines - a reversed list of already constructed lines
          ; (the list is reversed to avoid the use of "append" which makes the function very slow)
          (define (fmt/a words line lines)
            (cond
              [(empty? words) (cons line lines)]
              [else (local (; String
                            (define next-line (if (string=? line "")
                                                  (first words)
                                                  (string-append line " " (first words)))))
                      (cond
                        [(> (string-length next-line) w) (fmt/a (rest words)
                                                                (first words)
                                                                (cons line lines))]
                        [else (fmt/a (rest words) next-line lines)]))])))
    (reverse (fmt/a words0 "" '()))))

; N String String -> String
; arrange the words from in-f in the given order into lines of maximal width w,
; write these lines to out-f
(check-expect (read-file (fmt 60 "../assets/lorem-ipsum.txt" "../assets/tmp.txt"))
              (read-file "../assets/lorem-ipsum-w60.txt"))

(define (fmt w in-f out-f)
  (local (; [List-of String]
          (define words (read-words in-f))
          ; [List-of String]
          (define lines (fmt/l w words))
          ; String
          (define content (foldl (lambda (line result)
                                   (string-append result line "\n"))
                                 ""
                                 lines)))
    (write-file out-f content)))
