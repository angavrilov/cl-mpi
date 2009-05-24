 ;;; CFFI-Grovel is needed for processing grovel-file components
;;(cl:eval-when (:load-toplevel :execute)
;;  (asdf:operate 'asdf:load-op 'cffi-grovel))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))


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
     ;     (:file "mpi-grovel.grovel-tmp")
     ;     (:file "mpi" :depends-on ("packages" "mpi-grovel.grovel-tmp"))
     (:file "mpi" :depends-on ("packages"))
     ;;(:file "mpi-test" :depends-on ("mpi"))
     ))


