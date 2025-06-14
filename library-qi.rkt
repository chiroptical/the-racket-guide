#lang racket

(require qi)
(require racket/format)

(define-flow average (~> (-< + count) /))
(define-flow average-list (~> △ (-< + count) /))

(average 1 2 3)
(average-list '(1 2 3))

(define-flow match-fizz (~> (remainder 3) (= 0)))
(define-flow match-buzz (~> (remainder 5) (= 0)))

(define-flow match-fizzbuzz (~> (-< match-fizz match-buzz) &))

(define-flow fizzbuzz
             (>< (switch [match-fizzbuzz "fizzbuzz"]
                         [match-fizz "fizz"]
                         [match-buzz "buzz"]
                         [else ~v])))

(fizzbuzz 15 30 31)

(define-flow fizzbuzz-list
             (~> △
                 (>< (switch [match-fizz
                              (switch [match-buzz "fizzbuzz"] [else "fizz"])]
                             [match-buzz "buzz"]
                             [else ~v]))))
(fizzbuzz-list (build-list 100 (λ (x) (+ x 1))))
