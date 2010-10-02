(in-package :asdf)

(defclass mpicc-source-file (cl-source-file)
  ())

(defmethod perform :around ((op compile-op) (component mpicc-source-file))
  (let ((c::*CC* "mpicc")
        (c::*LD* "mpicc")
        (*features* (list* :ecl-mpicc-compile *features*)))
    (call-next-method)))

(defmethod perform :around ((op load-op) (component mpicc-source-file))
  (let ((*features* (list* :ecl-mpicc-compile *features*)))
    (call-next-method)))

