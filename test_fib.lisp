#!/usr/bin/sbcl --script

(require "asdf")
(load "fib.lisp")
(print (fib_naive 5))

(asdf:asdf-version)

(asdf:defsystem "binet"
  :components ((:file "fib.lisp")))

(print (fib_naive 5))
