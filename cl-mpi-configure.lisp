(in-package :mpi)
(defvar *mpi-header-file* "/usr/include/mpi/mpi.h")


(defun load-mpi-foreign-libraries ()
  (cffi:use-foreign-library "/usr/lib/mpich-shmem/lib/shared/libmpich-shmem.so") ; this should work for a standard MPICH imlementation
  ;;(cffi:use-foreign-library "libmpiskeleton.so.1.0.1"))
  )
;  (load-mpi-foreign-libraries)
  ;(format t "Loaded MPI libraries!~%")

