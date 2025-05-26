#lang racket

(define positive-number (and/c number? positive?))

(define/contract (add-positive-amounts x y)
  (-> positive-number positive-number positive-number)
  (+ x y))

; (add-positive-amounts 0 0)
; add-positive-amounts: contract violation
; expected: positive?
; given: 0
; in: an and/c case of
;     the 1st argument of
;     (->
;      (and/c number? positive?)
;      (and/c number? positive?)
;      (and/c number? positive?))
(add-positive-amounts 1 1) ; totally fine

; https://docs.racket-lang.org/guide/contracts.html
; https://docs.racket-lang.org/guide/contract-boundaries.html
