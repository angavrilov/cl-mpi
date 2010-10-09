(in-package :asdf)

;;; CFFI-Grovel is needed for processing grovel-file components
#-(and ecl (or mpicc mpicc-static))
(load-system :cffi-grovel)

#+(and ecl (or mpicc mpicc-static))
(load (merge-pathnames #P"mpicc-source-file.lisp" *load-truename*))

(defsystem cl-mpi
    :description "Common Lisp bindings for the Message Passing Interface (MPI)"
    :author "Alex Fukunaga"
    :version "0.1.0"
    :license "MIT"
    :depends-on (:cffi
                 #-(and ecl (or mpicc mpicc-static))
                 :cffi-grovel)
    :components
    #-(and ecl (or mpicc mpicc-static))
    ((:file "packages")
     (:file "cl-mpi-configure" :depends-on ("packages"))
     (:file "mpi-types" :depends-on ("packages"))
     (cffi-grovel:grovel-file "mpi-grovel" :depends-on ("packages" "cl-mpi-configure"))
     (:file "mpi-cffi-api" :depends-on ("packages" "mpi-grovel" "mpi-types"))
     (:file "mpi-bindings" :depends-on ("packages" "mpi-cffi-api"))
     (:file "mpi" :depends-on ("packages" "mpi-bindings")))
    #+(and ecl (or mpicc mpicc-static))
    ((:file "packages")
     (:file "cl-mpi-configure" :depends-on ("packages"))
     (:file "mpi-types" :depends-on ("packages"))
     (:file "mpi-ecl-api" :depends-on ("packages" "cl-mpi-configure" "mpi-types"))
     (mpicc-source-file "mpi-bindings" :depends-on ("packages" "mpi-ecl-api"))
     (:file "mpi" :depends-on ("packages" "mpi-bindings"))))

