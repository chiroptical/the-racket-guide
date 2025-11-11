#lang racket

; Exceptions

; catch an exception using `with-handlers`
(with-handlers ([exn:fail:contract:divide-by-zero? (lambda (_exn) +inf.0)])
  (/ 1 0))

; (error "raises exception")
(with-handlers ([exn:fail? (lambda (_exn) 'beep-boop)])
  (error "oh noes"))

; all exceptions raise a subtype of exn:fail?

; raise lets you throw a value as an exception
(with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) (+ v 2))])
  (raise 2))

(define (always-fail n)
  (with-handlers ([even? (lambda (_v) 'even)]
                  [positive? (lambda (_v) 'positive)])
    (raise n)))

(always-fail 2)
(always-fail 3)
; (always-fail -3) ; would throw

(with-handlers
    ([exn:fail? (lambda (v)
                  ; returns a function which can write to the current error port
                  ((error-display-handler) (exn-message v) v))])
  (car 17))

; https://docs.racket-lang.org/guide/prompt.html
; "prompts" and "aborts" are more primitive versions of raising and handling.

(define saved-k #f)
(define (save-comp!)
  (call-with-composable-continuation
   (lambda (k) ; k is the captured continuation
     (set! saved-k k)
     0)))

(+ 1 (+ 1 (+ 1 (save-comp!))))
; above, the saved continuation is (lambda (x) (+ 1 (+ 1 (+ 1 x))))

; you can see that by calling saved-k, it will add 3
(saved-k 3)
(saved-k 10)
(saved-k (saved-k 0))

; call-with-composable-continuation is determined dynamically, e.g.
(define (sum n)
  (if (zero? n)
      (save-comp!)
      (+ n (sum (sub1 n)))))
(sum 5)
; saved-k is (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 ?)))))
(saved-k 0)

; TODO: more details about these in https://docs.racket-lang.org/more/index.html
