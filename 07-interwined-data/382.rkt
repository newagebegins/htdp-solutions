;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |382|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; <machine initial="black">
;   <action state="black" next="white" />
;   <action state="white" next="black" />
; </machine>

'(machine ((initial "black"))
          (action ((state "black") (next "white")))
          (action ((state "white") (next "black"))))