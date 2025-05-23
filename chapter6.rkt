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
