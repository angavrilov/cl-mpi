(asdf:defsystem par-eval
   :description "simple work farming framework for CL-MPI"
    :author "Alex Fukunaga"
    :version "0.1.0"
    :license "MIT"
    :depends-on (:cl-mpi :cffi :cffi-grovel)
    :components
    ((:file "par-eval")))
