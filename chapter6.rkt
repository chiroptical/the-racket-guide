#lang racket

; Relative imports work
; (require "my-module/hello.rkt")
; (require "my-module/util/world.rkt")

; could also turn 'my-module' into a collection
; and import that instead
; To do that `raco pkg install --link my-module`
; it will look these up in the racket specific directory
; and pre-compile bytecode for the package
(require my-module/hello)
(require my-module/util/world)

; LOOKUP: https://docs.racket-lang.org/raco/setup.html
; Apparently, raco setup can be used to byte compile local dependencies.
; Sounds like you can have an `info.rkt` file to manage installation of a given
; project.

(hello)
(world)

; https://docs.racket-lang.org/guide/Module_Syntax.html

; #lang itself is actually syntactic sugar for the module
; form. Similar to how ' is shorthand for the quote form
; #lang wouldn't work in the repl though because it wraps
; the whole file. i.e. it is terminated by EOF
; See my-module/hello.rkt as an example
; This is useful in the REPL because you can define a module which isn't
; associated with a file.

; You can also next modules, i.e.
(module zoo racket
  (provide tiger)
  (define tiger "Tony"))
(require 'zoo)
tiger

; can also use module* form to provide "extra" exports
; should someone really need them
(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))
(module* extras #f
  (provide show))
; can get the extra submodule with
; (require submod "chapter6.rkt" extras)

; This will automatically run with `racket chapter6.rkt` because 'main' is a
; special case
(module* main #f
  (hello))

; module+ forms are similar to `module* name-id #f`
; but you can re-use the name, particular useful for tests
(module+ test
  (require rackunit)
  (define ε 1e-10))

(define (drop t)
  (* 1/2 9.8 t t))

(module+ test
  (check-= (drop 0) 0 ε))

(module+ test
  (check-= (drop 10) 490 ε))

; https://docs.racket-lang.org/guide/module-paths.html
