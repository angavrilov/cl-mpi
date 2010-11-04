(require :asdf)
(pushnew :mpicc-static *features*)
(asdf:load-system :cl-mpi-test)
(asdf:make-mpicc-build :cl-mpi-test :type :program :move-here "."
                       :epilogue-code '(progn (mpi::testmpi) (ext:quit)))
(ext:quit)
