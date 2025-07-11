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

; I know nothing about interfaces at this point, but apparently
; this compiles
(define area-container-window<%> (interface ()))

; the function accepts a single argument 'parent'
; the domain must be an object matching the interface 'area-container-window<%>'
; the range contract only changes the children of parent
; the expression following `_` will run when the contract is called
; instead of when it returns! i.e. `get-children` is run before the function returns
; the result is passed into the lambda as `child` and we check that the old children
; plus the new child is the same as the current children.
(->i ([parent (is-a?/c area-container-window<%>)])
     [_
      (parent)
      (let ([old-children (send parent get-children)])
        (λ (child)
          (andmap eq?
                  (append old-children (list child))
                  (send parent get-children))))])

(define (split l)
  (define (split l w)
    (cond
      [(null? l) (values (list->string (reverse w)) '())]
      [(char=? #\newline (car l)) (values (list->string (reverse w)) (cdr l))]
      [else (split (cdr l) (cons (car l) w))]))
  (split l '()))

(provide (contract-out [split
                        (-> (listof char?) (values string? (listof char?)))]))

; note: a string is an array by default, but you can convert it to a charlist
(split (string->list "hello\nworld"))

(define (substring-of? s)
  (flat-named-contract (format "substring of ~s" s)
                       (lambda (s2)
                         (and (string? s2)
                              (<= (string-length s2) (string-length s))
                              (equal? (substring s 0 (string-length s2)) s2)))))

(define (split/1 l)
  (define (split/1 l w)
    (cond
      [(null? l) (values (list->string (reverse w)) '())]
      [(char=? #\newline (car l)) (values (list->string (reverse w)) (cdr l))]
      [else (split/1 (cdr l) (cons (car l) w))]))
  (split/1 l '()))

; (provide (contract-out [split/1
;                         (->i ([fl (listof char?)])
;                              (values [s (fl) (substring-of? (list->string fl))]
;                                      [c (fl) (substring-of? (list->string fl))]))]))

(provide (contract-out
          [split/1
           (->i ([fl (listof char?)])
                (values [s
                         (fl)
                         ; string-len/c requires the length to be less
                         ; than this. So, if the charlist doesn't contain
                         ; a newline it will return 'fl' i.e. you need
                         ; to add 1 otherwise the contract would fail
                         ; if you gave (string->list "hello")
                         (string-len/c (+ 1 (length fl)))]
                        [c (listof char?)]))]))

(split/1 (string->list "hello\nworld"))

(define (n-step proc inits)
  (let ([inc (apply proc inits)])
    (when inc
      (n-step proc (map (λ (x) (+ x inc)) inits)))))

(define (i x)
  (printf "~s\n" x)
  (if (= x 0) #f -1))
(n-step i '(2))

(define (j x y)
  (define z (+ x y))
  (printf "~s\n" (list x y z))
  (if (= z 0) #f -1))

(n-step j '(1 1))

(provide (contract-out
          [n-step
           (->i ([proc
                  (inits)
                  (and/c (unconstrained-domain-> (or/c #f number?))
                         (λ (f) (procedure-arity-includes? f (length inits))))]
                 [inits (listof number?)])
                ()
                any)]))

(define (increase f x y)
  (f x y))

(provide (contract-out
          [increase
           (->i ;[f (and/c (unconstrained-domain-> number?) (λ (f) (procedure-arity-includes? f 2)))] ; this effectively codes the same contract, but uses unconstrained-domain->
            ([f (-> number? number? number?)] [x number?] [y number?])
            ()
            (lambda (x y) (>/c (+ x y))))]))

(increase (lambda (x y) (* 2 x y)) 1 2) ; would work

(increase (lambda (x y) (* x y)) 1 2) ; wouldn't work

; Section 7.4

;; TODO: Could be nice to implement this such that you don't have to re-compute
;; (f acc) each time
(define (my-argmax f lov)
  (match lov
    ['() (error "...")]
    [(cons hd tl) (foldr (λ (acc x) (if (> (f acc) (f x)) acc x)) hd tl)]))

(my-argmax identity '(1 2 4 4 3))

(provide (contract-out [my-argmax
                        (-> (-> any/c
                                ; number? is not comparable with <
                                ; LOOKUP: why?
                                real?)
                            ; This embodies non-empty list
                            (and/c pair? list?)
                            any/c)]))

; check out version 2 iteration next! https://docs.racket-lang.org/guide/contracts-first.html
