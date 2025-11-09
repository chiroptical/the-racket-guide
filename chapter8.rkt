#lang racket

; Input and Output
; https://docs.racket-lang.org/guide/i_o.html

(define filename "example.txt")

(call-with-output-file filename
                       #:exists 'truncate ; replace the file's contents
                       (lambda (out) (display "hello" out)))

(call-with-input-file filename (lambda (in) (read-line in)))

; open port to accumulate data into a string
(define p (open-output-string))
(display "foo" p)
(display "bar" p)
(get-output-string p)
(display "baz" p)
(get-output-string p)

; tcp ports example
(define server (tcp-listen 12345))
(define-values (_c-in c-out) (tcp-connect "localhost" 12345))
(define-values (s-in _s-out) (tcp-accept server))
(display "hello\n" c-out) ; send "hello\n" to the server
(close-output-port c-out) ; close the tcp port, we are done
(read-line s-in) ; hello
(read-line s-in) ; #<eof>

; subprocess exists as a port, e.g.
(define-values (_ stdout stdin stderr) (subprocess #f #f #f "/usr/bin/wc" "-w"))
(display "a b c\n" stdin)
(close-output-port stdin)
(read-line stdout)
(close-input-port stdout)
(close-input-port stderr)

; make-pipe returns two ports that are ends of a pipe, e.g.
(define-values (in out) (make-pipe))
(display "garbage" out)
(close-output-port out)
(read-line in)

; Section 8.2 https://docs.racket-lang.org/guide/default-ports.html

(define (swing-hammer)
  (display "Ouch!" (current-error-port)))

(swing-hammer) ; Ouch! to error port

; you can use parametrize to change the behavior, e.g.

(let ([s (open-output-string)])
  (parameterize ([current-error-port
                  s]) ; parameterize the error port as an output string port
    (swing-hammer)
    (swing-hammer)
    (swing-hammer))
  (get-output-string s)) ; Ouch!Ouch!Ouch! to current-output-port as string

; print - expression layer Racket syntax view
; write - reader layer, i.e. write followed by read often works
; display - character layer
; Sometimes all three are the same, more often not
(define (deliver who when what)
  (printf "Items ~a for shopper ~s: ~v" who when what))
; ~a display
; ~s write
; ~v print
(deliver '("list") '("John") '("milk"))

(define-values (pipe-in pipe-out) (make-pipe))
(write "hello" pipe-out)
(read pipe-in)

; Section 8.4 https://docs.racket-lang.org/guide/serialization.html

; default structs will not serialize and deserialize over a pipe

(struct posn (x y) #:transparent)

(write (posn 1 2)) ; #(struct:posn 1 2)

(define-values (in- out-) (make-pipe))

(write (posn 1 2) out-)

(define v (read in-))

(write (posn? v)) ; false
(write (vector? v)) ; true

(require racket/serialize)

(serializable-struct posn- (x y) #:transparent)
(deserialize (serialize (posn- 1 2)))

(define-values (-in -out) (make-pipe))
(write (serialize (posn- 1 2)) -out)
(deserialize (read -in)) ; this works fine!

; Section 8.5 https://docs.racket-lang.org/guide/encodings.html
