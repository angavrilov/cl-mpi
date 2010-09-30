 ;;; CFFI-Grovel is needed for processing grovel-file components
(asdf:load-system :cffi-grovel)

(asdf:defsystem cl-mpi
    :description "Common Lisp bindings for the Message Passing Interface (MPI)"
    :author "Alex Fukunaga"
    :version "0.1.0"
    :license "MIT"
    :depends-on (:cffi :cffi-grovel)
    :components
    ((:file "packages")
     (:file "cl-mpi-configure" :depends-on ("packages"))
     (cffi-grovel:grovel-file "mpi-grovel" :depends-on ("packages" "cl-mpi-configure"))
     (:file "mpi-bindings" :depends-on ("packages" "mpi-grovel"))
     (:file "mpi" :depends-on ("packages" "mpi-bindings"))))

