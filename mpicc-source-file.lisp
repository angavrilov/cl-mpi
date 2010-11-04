(in-package :asdf)

(defclass mpicc-source-file (cl-source-file)
  ())

(defmethod perform :around ((op compile-op) (component mpicc-source-file))
  (let ((c::*CC* "mpicc")
        (c::*LD* #-mpicc-static "mpicc"
                 ;; Link will fail, so force the
                 ;; compiler to use a no-op:
                 #+mpicc-static "echo")
        (*features* (list* :ecl-mpicc-compile *features*)))
    (call-next-method)
    ;; Simulate creating the fasl to avoid spurious recompilations:
    #+mpicc-static
    (with-open-file (stream (second (output-files op component))
                            :if-exists :supersede :direction :output)
      (princ "STUB" stream))))

(defmethod perform :around ((op load-op) (component mpicc-source-file))
  (let ((*features* (list* :ecl-mpicc-compile *features*)))
    (call-next-method)))

#+mpicc-static
(defmethod perform ((o load-op) (c mpicc-source-file))
  (declare (ignore o))
  ;; In static mode load the source, and hope that
  ;; nobody actually calls the functions inside it
  ;; until the whole program is fully linked.
  (perform (make-instance 'load-source-op) c))

(export 'make-mpicc-build)

(defun make-mpicc-build (system &rest args)
  (let ((c::*LD* "mpicc"))
    (apply #'make-build system args)))
