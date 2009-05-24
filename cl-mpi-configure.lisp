(in-package :mpi)
;(defvar *mpi-header-file* "/usr/include/mpi/mpi.h") ; MPICH 1.2 ubuntu apt-get install
(defvar *mpi-header-file* "/usr/local/include/mpi.h") ; MPICH2 xeon8
;(defvar *mpi-header-file* "/usr/local/include/mpi.h") ; OPENMPI1.3 xeon8
;(defvar *mpi-header-file* "/usr/include/mpi.h")


;; Depending on the MPI implementation, uncomment one of the lines below

(pushnew :mpich2 *features*) ; Use MPICH2
;(pushnew :mpich1 *features*) ; Use MPICH 1.X
;(pushnew :openmpi *features*) ; Use OpenMPI

(defparameter *mpi-string-buf-size* 60000 "size of the string buffer that is allocated byt string send/receive functions mpi-send-string mpi-receive-string and mpi-send-receive-string")
(declaim (fixnum *mpi-string-buf-size*))

(defun load-mpi-foreign-libraries ()
  ;;(cffi:use-foreign-library "/usr/lib/mpich-shmem/lib/shared/libmpich-shmem.so") ; ; MPICH 1.2 ubuntu apt-get install

  ;; For MPICH2, Need to load a special stub shared object
  (cffi:use-foreign-library "/home/fukunaga/cl-mpi/mpich2-stub/libmpiskeleton.so.1.0.1") ; this should work for a standard OpenMPI imlementation
  ;;(cffi:use-foreign-library "libmpiskeleton.so.1.0.1")) ; TSUBAME

  ;;(cffi:use-foreign-library "/usr/lib/openmpi/lib/libmpi.so") ; this should work for a standard OpenMPI imlementation
  ;;(cffi:use-foreign-library "/usr/local/lib/libmpi.so") ; OPENMPI1.3 xeon8
  )

