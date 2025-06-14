#lang racket

(require qi)
(require racket/format)

(define spacer (make-string 10 #\-))

(define-flow average (~> (-< + count) /))
(define-flow average-list (~> △ (-< + count) /))

(average 1 2 3)
(average-list '(1 2 3))

spacer

(define-flow match-fizz (~> (remainder 3) (= 0)))
(define-flow match-buzz (~> (remainder 5) (= 0)))

(define-flow match-fizzbuzz (~> (-< match-fizz match-buzz) &))

; This is slightly more flexible version because it accepts
; (match-match-fizzbuzz-list 1 2 3)
(define-flow match-fizzbuzz-list (>< (~> (-< match-fizz match-buzz) &)))

(define-flow fizzbuzz-naive
             (>< (switch [match-fizzbuzz-list "fizzbuzz"]
                         [match-fizz "fizz"]
                         [match-buzz "buzz"]
                         [else ~v])))

(fizzbuzz-naive 15 30 31)

spacer

(define-flow fizzbuzz-list
             (~> △
                 (>< (switch [match-fizz
                              (switch [match-buzz "fizzbuzz"] [else "fizz"])]
                             [match-buzz "buzz"]
                             [else ~v]))))
(fizzbuzz-list (build-list 10 (λ (x) (+ x 1))))

spacer

(define-flow fizzbuzz-simple
             (>< (if match-fizz
                     (if match-buzz "fizzbuzz" "fizz")
                     (if match-buzz "buzz" ~v))))
(fizzbuzz-simple 3 5 15 31)
