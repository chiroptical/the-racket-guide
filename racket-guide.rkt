#lang racket

; load this in the repl with
; (enter! "hello.rkt")
(define (extract str)
  (substring str 4 7))

; you can also run this as a script with `racket hello.rkt`
(extract "the cat out of the bag")

; Next https://docs.racket-lang.org/guide/to-scheme.html

; #t or #f for true/false, all non-#f are treated as true

; Note, `string-append` and `flavor` here are evaluated
; as separate expressions. So, the return of this function is
; just the constant "jello"
(define (nobake flavor)
  string-append
  flavor
  "jello")

(if #t "true" "false")

(cond
  [#f "nope"]
  [#f "nope"]
  [else "yup"])

(define (twice f x)
  (f (f x)))

(twice (lambda (x) (string-append x "!")) "hello")

; If you stick x in parens, it will try to evaluate it
; as a function
(let ([x (random 4)]
      [y (lambda (z) (* z 2))])
  (y x))

(define (my-length l)
  (cond
    [(empty? l) 0]
    [else (+ 1 (my-length (rest l)))]))

(my-length (list 1 2 3))

(define (swap-two l)
  (match l
    [(cons m (cons n rest)) (cons n (cons m rest))]
    [_ l]))

(swap-two (list 1))
(swap-two (list 1 2))
(swap-two (list 1 2 3))

; https://docs.racket-lang.org/guide/datatypes.html

(= 2 (+ 1 1))

(boolean? #t)

(boolean? "no")

; Reminder that everything that isn't #f is truthy
(if "no" 1 0)

(integer->char 65)

(char->integer #\A)

#\u03BB

; a symbol is an atomic value that prints like
; an identifier preceded with leading '
(symbol? 'a)
(eq? 'a 'a)
(eq? 'a (string->symbol "a"))
(string->symbol "one, two")
; '|one, two|
(string->symbol "6")
; '|6|

; a keyword is similar to a symbol but is prefixed with #:
(string->keyword "apple")
(keyword? '#:apple)

; note vectors exist, fixed length, constant time access/update
#("x" "y" "z")
#4(baldwin bruce) ; fills with final value
#1() ; unable to add more than one element though, compile error

(define ht (make-hash))
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
(hash-ref ht "apple")
; '(red round)
; (hash-ref ht "coconut") ; would fail with...
; hash-ref: no value found for key
;   key: "coconut"
(hash-ref ht "coconut" "not there")

; https://docs.racket-lang.org/guide/scheme-forms.html

(define (anti-sum lst)
  (apply - 0 lst)) ; the 0 is prepended to the lst

(anti-sum '(1 2 3))

; https://docs.racket-lang.org/guide/lambda.html

; This version must match arity, we only give one argument
; so it won't compile
; ((lambda (x y) (+ x y)) 1)

; This version accepts any number of arguments ; which are first put into a list
((lambda x x) 1 2 3)
; '(1 2 3)

; this version accepts one argument followed many
(lambda (num . nums) (apply max (map magnitude (cons num nums))))

; this version takes one argument and an optional one
(define greet (lambda (given [surname "Smith"]) (string-append "Hello, " given " " surname)))

(greet "barry")
(greet "barry" "moo")

(define greet_
  (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
    (string-append hi ", " given " " surname)))

(greet_ #:hi "hello" "barry")
(greet_ "barry" #:hi "hello")
(greet_ #:hi "hello" "barry" #:last "moo")

; this one doesn't support keyword or optional arguments
(define greet/
  (case-lambda
    [(name) (string-append "Hello, " name)]
    [(given surname) (string-append "Hello, " given " " surname)]
    [(given surname hi) (string-append hi ", " given " " surname)]))

(greet/ "one")
(greet/ "one" "two")
(greet/ "one" "two" "three")

; . l means "rest"
(define (avg . l)
  (/ (apply + l) (length l)))

(avg 1 2 3)

(define (sum-tail _h . l)
  (apply + l))

(sum-tail 1 2 3)

; short-hand for curried function
(define ((make-add-suffix s2) s)
  (string-append s s2))

((make-add-suffix "!") "hello")

(define louder (make-add-suffix "!"))
(define question (make-add-suffix "?"))

(louder "hello")
(question "hello")

; returns a "values" form, it will appear on two lines
(quotient/remainder 13 3)

; you can capture each with
(define-values (quot rem) (quotient/remainder 13 3))
(printf "quot is ~s~n" quot)
(printf "rem is ~s~n" rem)

; let does "in parallel" binding, i.e. no references between bindings allowed
; let* does sequential binding
; letrec makes bindings available in all other bindings
; let-values, let-values*, and letrec-values also exist

; https://docs.racket-lang.org/guide/conditionals.html

(member "Groucho" '("Harpo" "Zeppo")) ; #f

(if (member "Groucho" '("Harpo" "Zeppo")) 'yep 'nope)

(cond
  [(= 2 3) (error "wrong!")]
  [(= 2 2) 'ok]
  [else (error "also wrong!")])

; first -> car
; rest -> cdr
(define (got-milk? lst)
  (cond
    [(null? lst) #f]
    [(eq? 'milk (first lst)) #t]
    [else (got-milk? (rest lst))]))

(got-milk? '())
(got-milk? '(apple banana))
(got-milk? '(apple milk banana))

(define (after-groucho lst)
  (cond
    [; note: member returns the location of "Groucho" and the rest of the list
     ; so => calls 'rest' on '("Groucho" "Zeppo")
     (member "Groucho" lst)
     =>
     cdr]
    [else (error "not there")]))

(after-groucho '("Harpo" "Groucho" "Zeppo"))

'hello ; a constant, same as
(quote hello)

; The unquote expressions are evaluated and then quoted
(quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
; '(1 2 3 4)

(let ([v (random 6)])
  (printf "~a\n" v)
  (case v
    [(0) 'zero]
    [(1) 'one]
    [(2) 'two]
    [(3 4 5) 'many])) ; matches multiple patterns, could use else here

; https://docs.racket-lang.org/guide/define-struct.html

(struct posn (x y))
(define a (posn 1 2))
(posn? a)
(posn? 1)
(posn-x a) ; get the x field of the struct

(define b (struct-copy posn a [x 3]))
(list (posn-x b) (posn-y b)) ; copy a to b overriding x by 3

(struct 3d-posn posn (z)) ; extend a 2d posn with a z dimension

(define c (3d-posn 1 2 3))
(3d-posn-z c)
; (3d-posn-x c) doesn't work because 3d-posn is a subtype of posn
; note: it won't even compile because it isn't a valid identifier
(posn-x c) ; works though

; the default print style for a struct is "opaque",
; i.e. #<posn> or #<3d-posn>
; However, you can make a transparent type like this
(struct t-posn (x y) #:transparent)
(define d (t-posn 1 2))
d ; like (t-posn 1 2)

; transparent structures equal? recurs on fields
; however, instance identity is required for opaque structures
(equal? (t-posn 1 2) (t-posn 1 2)) ; #t
(define e (posn 1 2))
(equal? e e) ; #t
(equal? e (posn 1 2)) ; #f

; you can recover the transparent behavior on opaque types with
(struct ex-posn (x y)
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (ex-posn-x a) (ex-posn-x b)) (equal?-recur (ex-posn-y a) (ex-posn-y b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (ex-posn-x a)) (* 3 (hash-recur (ex-posn-y a)))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (ex-posn-x a)) (* 3 (hash2-recur (ex-posn-y a)))))])

(equal? (ex-posn 1 2) (ex-posn 1 2)) ; #t, i.e. we recover the transparent behavior

; https://docs.racket-lang.org/guide/define-struct.html#(part._.Structure_.Type_.Generativity)

; Skipped ahead a bit because I was curious
; https://docs.racket-lang.org/guide/contract-boundaries.html

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
; (add-positive-amounts 1 1) ; totally fine
; note commenting for now to avoid printing
; can uncomment when you reach this section in the book
