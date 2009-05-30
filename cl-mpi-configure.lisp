(in-package :mpi)

(defparameter *mpi-string-buf-size* 60000 "size of the string buffer that is allocated byt string send/receive functions mpi-send-string mpi-receive-string and mpi-send-receive-string")
(declaim (fixnum *mpi-string-buf-size*))

;;sample Configuration for MPICH1.2.  The definitions below are where apt-get install mpich-shmem-bin places the header and library files.
;;#+nil  
(progn
  (pushnew :mpich1 *features*) ; Use MPICH 1.X
  (defvar *mpi-header-file* "/usr/include/mpi/mpi.h") ; this is where  ubuntu apt-get install puts mpi.h. 
  (defun load-mpi-foreign-libraries ()
    (cffi:use-foreign-library "/usr/lib/mpich-shmem/lib/shared/libmpich-shmem.so")) 
  )


;; sample configuration for MPICH2 (compiled and installed locally)
#+nil 
(progn
  (pushnew :mpich2 *features*) ; Use MPICH2
  (defvar *mpi-header-file* "/usr/local/include/mpi.h") 

  ;; For MPICH2, Need to load a special stub shared object, and not the MPICH shared library directly
  (defun load-mpi-foreign-libraries ()
    (cffi:use-foreign-library "/home/fukunaga/cl-mpi/mpich2-stub/libmpiskeleton.so.1.0.1"))
  )



