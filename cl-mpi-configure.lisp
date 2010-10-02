(in-package :mpi)

;;; Helper code to determine library paths from
;;; environment at compile time. This assumes the
;;; same module conventions as C compilation.

(eval-when (:compile-toplevel :execute)
  ;; Helper functions
  (defun get-env-value (name &optional default)
    (or #+CMU (cdr (assoc name ext:*environment-list* :test #'string=))
        #+Allegro (sys:getenv name)
        #+CLISP (ext:getenv name)
        #+ECL (si:getenv name)
        #+SBCL (sb-unix::posix-getenv name)
        #+LISPWORKS (lispworks:environment-variable name)
        default))
  (defun directory-pathname (name)
    (if name (pathname (concatenate 'string name "/"))))
  (defun split-path (path)
    (loop
       for rpath = (concatenate 'string path ":")
       then (subseq rpath (1+ index))
       for index = (position #\: rpath)
       for item = (subseq rpath 0 index)
       while index
       unless (string= item "") collect (directory-pathname item)))
  (defun find-in-path (path &rest pathnames)
    (dolist (prefix path)
      (dolist (target pathnames)
        (let ((truename (probe-file (merge-pathnames target prefix))))
          (when truename
            (return-from find-in-path truename)))))
    (error "Could not find any of ~S in path ~S" pathnames path))
  ;; Find mpicc in path
  (defparameter *mpicc-name*
    (loop
       for item in (split-path (get-env-value "PATH" ""))
       for name = (merge-pathnames #P"mpicc" item)
       for truename = (probe-file name)
       when truename return (list truename)))
  ;; Search path for includes and libs
  (defparameter *mpi-include-path*
    (let ((mpiinc (get-env-value "MPI_INCLUDE")))
      (append
       ;; MPI_INCLUDE has precedence
       (if mpiinc (list (directory-pathname mpiinc)))
       ;; Then try a path derived from mpicc if found.
       (mapcar (lambda (x) (merge-pathnames #P"../include/" x)) *mpicc-name*)
       ;; Hard-coded paths
       (list #P"/usr/include/mpi/"))))
  (defparameter *mpi-lib-path*
    (let ((truename (or *compile-file-truename* *load-truename*))
          (mpilib (get-env-value "MPI_LIB")))
      (append
       ;; Use the stub library if built
       (if truename (list (merge-pathnames #P"mpich2-stub/" truename)))
       ;; Use MPI_LIB if defined
       (if mpilib (list (directory-pathname mpilib)))
       ;; Try deriving a path from mpicc name
       (mapcar (lambda (x) (merge-pathnames #P"../lib/" x)) *mpicc-name*)
       ;; Look in LD_LIBRARY_PATH
       (split-path (get-env-value "LD_LIBRARY_PATH" ""))
       ;; Hard-coded paths
       (list #P"/usr/lib/mpich-shmem/lib/")))))

;;;
;;; Actual configuration, with defaults guessed from the environment.
;;;

(declaim (fixnum *mpi-string-buf-size*))
(defparameter *mpi-string-buf-size* 60000
  "Size of the string buffer that is allocated by the string send/receive functions")

(defvar *mpi-header-file*
  #.(find-in-path *mpi-include-path* #P"mpi.h"))

(defvar *mpi-shared-library*
  #.(find-in-path *mpi-lib-path*
                  #P"libmpiskeleton.so.1.0.1"
                  #P"libmpich.so"
                  #P"libmpich.so.1"
                  #P"shared/libmpich.so"
                  #P"shared/libmpich.so.1"
                  #P"libmpich-shmem.so"
                  #P"shared/libmpich-shmem.so"
                  #P"libmpi.so"
                  #P"libmpi.so.0"
                  #P"shared/libmpi.so"
                  #P"shared/libmpi.so.0"))

(defun load-mpi-foreign-libraries ()
  #-(and ecl mpicc)
  (cffi:load-foreign-library *mpi-shared-library*))

(let ((path (string-downcase (namestring *mpi-shared-library*))))
  (cond ((search "/openmpi" path)
         (pushnew :openmpi *features*))
        ((search "/mpich2" path)
         (pushnew :mpich2 *features*))
        ((search "/mpich" path)
         (pushnew :mpich1 *features*))
        (t
         (warn "Could not guess the MPI implementation type."))))

