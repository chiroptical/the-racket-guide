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

; > Racket encourages contracts mainly at module boundaries
; typically modules are the boundaries where we enforce these
; contracts

(module+ server
  (provide (contract-out [amount (and/c number? positive?)]))
  (define amount 150))

(module+ main
  (require (submod ".." server))
  (+ amount 10))

; define/contract is useful if you need finer grained control
; than modules for contracts

; Reminder f : A -> B defines a function with "domain" A and "range" B

(number? . -> . any)
; is equivalent to
(-> number? any)

; using define/contract will check the contract for every call
; of the function whether inside or outside of a module.

(define (f x)
  (values (+ x 1) (- x 1)))
(f 1)
; this function would satisfy (-> integer? any) because it produces multiple
; values
; it would not satisfy (-> integer? any/c) because any/c can only return one
; value

(define (amount? a)
  (and (number? a) (integer? a) (exact? a) (>= a 0)))
; a is a non-negative integer

; (provide
;  (contract-out
;   ....
;   ; convert an  amount (natural number) of cents
;   ; into a dollar-based string
;   [format-nat (-> natural-number/c
;                   (and/c string? #rx"[0-9]*\\.[0-9][0-9]"))]))
; can use regex inside contracts

; flat-named-contract allows you to attach names to contracts

; ->* is used for optional arguments, first parenthesized group
; is required arguments, second is parenthesized optional arguments
; and the last one is the output

(define (max-abs n . rst)
  (foldr (lambda (n m) (max (abs n) m)) (abs n) rst))

; (provide (contract-out [max-abs (-> real? real? ... real?)]))
(provide (contract-out [max-abs (-> real? real? real?)]))

; this **won't** fail for internal calls, but will for external calls
; would need to use define/contract to get internal calls to fail
(max-abs 1 2 3 4 5)

; alternately could use
; (provide
; (contract-out
;  [max-abs (->* (real?) () #:rest (listof real?) real?)]))

; keyword arguments are specified with #:keyword type
; the optional syntax works the same

(define report-cost
  (case-lambda
    [(lo hi) (format "between $~a and $~a" lo hi)]
    [(desc) (format "~a of dollars" desc)]))

(provide (contract-out [report-cost
                        (case-> ; case 1
                         (integer? integer? . -> . string?)
                         ; case 2
                         (string? . -> . string?))]))

; ->i means "indy dependent" contract. The output type is dependent
; on the input type

; for example, you could define a withdraw function where the account balance
; has to be higher than the amount to withdraw
; (provide (contract-out [withdraw
;    (->i ([acc account?] [amt (acc) (and/c amount/c (<=/c (balance acc)))])
;         [result
;          (acc amt)
;          (and/c account?
;                 (lambda (res) (>= (balance res) (- (balance acc) amt))))])]))

; (->i ([parent (is-a?/c area-container-window<%>)])
;      [_
;       (parent)
;       (let ([old-children (send parent get-children)])
;         (λ (child) (andmap eq? (append old-children (list child)) (send parent get-children))))])

(define x '())
(define (get-x)
  x)
(define (g)
  (set! x (cons 'g x)))
(provide (contract-out [g
                        (->i ()
                             [_
                              ()
                              (begin
                                (set! x (cons 'ctc x))
                                any/c)])]
                       [get-x (-> (listof symbol?))]))

; https://docs.racket-lang.org/guide/contracts-general-functions.html
; 7.3.7 is mega confusing...
