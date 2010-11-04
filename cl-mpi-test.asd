(in-package :asdf)

(defsystem cl-mpi-test
    :description "Tests for Common Lisp MPI bindings"
    :author "Alex Fukunaga"
    :license "MIT"
    :depends-on (:cl-mpi :par-eval)
    :components
    ((:file "mpi-test")))
