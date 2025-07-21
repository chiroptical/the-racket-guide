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

; version 1
; (provide (contract-out [my-argmax
;                         (-> (-> any/c
;                                 ; number? is not comparable with <
;                                 ; LOOKUP: why?
;                                 real?)
;                             ; This embodies non-empty list
;                             (and/c pair? list?)
;                             any/c)]))

; version 2
; (provide (contract-out [my-argmax
;                         (->i ([f (-> any/c real?)] [lov (and/c pair? list?)])
;                              ()
;                              (r (f lov)
;                                 (λ (r)
;                                   (define f@r (f r))
;                                   (for/and ([v lov])
;                                     (>= f@r (f v))))))]))

; version 2, rev a.
; memq is like pointer equality ; This contract says, the maximum is one of the
; elements and it is greater than or equal to all elements
; (provide (contract-out [my-argmax
;                         (->i ([f (-> any/c real?)] [lov (and/c pair? list?)])
;                              ()
;                              (r (f lov)
;                                 (lambda (r)
;                                   (define f@r (f r))
;                                   (and (memq r lov)
;                                        (for/and ([v lov])
;                                          (>= f@r (f v)))))))]))

; version 3
; the output 'r' is greater than or equal to the other elements
; additionally, the first element which equals 'v' is 'r'.
; I think this eq? should be memq?
; (provide (contract-out
;           [my-argmax
;            (->i ([f (-> any/c real?)] [lov (and/c pair? list?)])
;                 ()
;                 (r (f lov)
;                    (lambda (r)
;                      (define f@r (f r))
;                      (and (for/and ([v lov])
;                             (>= f@r (f v)))
;                           (eq? (first (memf (lambda (v) (= (f v) f@r)) lov))
;                                r)))))]))

; the book breaks this up into smaller components, i.e.
; `dominates-all` is the first check and `is-first-max?` the second
; it then points out we've introduces some inefficiency

; version 3, rev b.
(provide (contract-out [my-argmax
                        (->i ([f (-> any/c real?)] [lov (and/c pair? list?)])
                             ()
                             (r (f lov)
                                (lambda (r)
                                  (define f@r (f r))
                                  (define flov (map f lov))
                                  (and (is-first-max? r f@r (map list lov flov))
                                       (dominates-all f@r flov)))))]))

; f@r is greater or equal to all f@v in flov
(define (dominates-all f@r flov)
  (for/and ([f@v flov])
    (>= f@r f@v)))

; r is (first x) for the first
; x in lov+flov s.t. (= (second x) f@r)
(define (is-first-max? r f@r lov+flov)
  (define fst (first lov+flov))
  (if (= (second fst) f@r)
      (eq? (first fst) r)
      (is-first-max? r f@r (rest lov+flov))))

; The guide then goes through some explanation to point out
; that argmax doesn't even need to call `f` for a singleton list.
; Additionally, if `f` contains effects you'll see them twice because
; of how contracts work. It generalizes the above contracts to properly
; handle the singleton case.

(my-argmax (λ (x) x) '(1 2 3))

; Section 7.5

(struct posn [x y])

(define origin (posn 0 0))

(provide (contract-out [origin (struct/c posn zero? zero?)]))

(provide (contract-out (struct posn ((x number?) (y number?)))
                       [p-okay posn?]
                       [p-sick posn?]))

(define p-okay (posn 10 20))
(define p-sick (posn 'a 'b))

; Note: internally, 'p-sick' would be totally fine from a contract perspective
; if you simply used it as a constant. However, if you did `(posn-x p-sick)`
; it would fail outside this module. This is because `posn-x` called outside
; this module passes over the contract surface. We can prevent this by
; being more specific about the types of `p-okay` and `p-sick`, i.e.
; `[p-sick (struct/c posn number? number?)]`

; Section 7.5.3

; a binary search tree example
(struct node (val left right))

; determines if `n' is in the binary search tree `b',
; exploiting the binary search tree invariant
(define (in? n b)
  (cond
    [(null? b) #f]
    [else
     (cond
       [(= n (node-val b)) #t]
       [(< n (node-val b)) (in? n (node-left b))]
       [(> n (node-val b)) (in? n (node-right b))])]))

; a predicate that identifies binary search trees
(define (bst-between? b low high)
  (or (null? b)
      (and (<= low (node-val b) high)
           (bst-between? (node-left b) low (node-val b))
           (bst-between? (node-right b) (node-val b) high))))

(define (bst? b)
  (bst-between? b -inf.0 +inf.0))

(provide (struct-out node))
(provide (contract-out [bst? (any/c . -> . boolean?)]
                       ; [in? (number? bst? . -> . boolean?)]
                       ))

; in this iteration, bst-bst-between? searches through the whole tree
; for every call to `in`, i.e. we lose the logarithmic complexity of
; search in our contract!

; bst-between : number number -> contract
; builds a contract for binary search trees
; whose values are between low and high
(define (bst-between/c low high)
  (or/c null?
        (struct/dc node
                   [val (between/c low high)]
                   [left (val) #:lazy (bst-between/c low val)]
                   [right (val) #:lazy (bst-between/c val high)])))

(define bst/c (bst-between/c -inf.0 +inf.0))

(provide (struct-out node))
(provide (contract-out [bst/c contract?] [in? (number? bst/c . -> . boolean?)]))

; Note: `define-opt/c` also exists and supposedly can speed up the body
; of this function.
; LOOKUP: How does it speed up the body of the contract?

; Next: https://docs.racket-lang.org/guide/contracts-exists.html
