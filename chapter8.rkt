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
