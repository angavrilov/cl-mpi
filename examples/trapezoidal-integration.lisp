(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'asdf)
  (asdf:operate 'asdf:load-op 'cl-mpi))

(defun integrate (f a b n)
  "Numerical integration using the trapezoidal rule"
  (let ((h (/ (- b a) n))
	(integral (/ (+ (funcall f a) (funcall f b)) 2.0))
	(x a))
    (loop for i from 1 to (1- n) do
	  (incf x h)
	  (incf integral (funcall f x)))
    (* integral h)))

(defun trapezoidal-integration (f a b n)
  "Parallel, numerical integration using the trapezoidal rule"
  (let* ((my-rank (mpi:mpi-comm-rank))
	 (comm-size (mpi:mpi-comm-size))
	 (h (/ (- b a) n))
	 (local-n (floor (/ n comm-size)))
	 (local-a (+ a (* my-rank local-n h)))
	 (local-b (+ local-a (* local-n h)))
	 (local-integral  (integrate f local-a local-b local-n)))

    (cond ((= 0 my-rank)
	   (let ((total-integral (+ local-integral 
				    (loop for source from 1 below comm-size summing
				       (mpi:mpi-receive-auto source)))))
	     (format t "Integral is ~a~%" total-integral)))
	  (t ; my-rank != 0
	   (mpi:mpi-send-auto local-integral 0))))
  )

(defun trapezoidal-integration-with-reduce (f a b n)
  "Parallel, Numerical integration using the trapezoidal rule. 
   Uses MPI_Reduce to sum the integrals"
  (let* ((my-rank (mpi:mpi-comm-rank))
	 (comm-size (mpi:mpi-comm-size))
	 (h (/ (- b a) n))
	 (local-n (floor (/ n comm-size)))
	 (local-a (+ a (* my-rank local-n h)))
	 (local-b (+ local-a (* local-n h)))
	 (local-integral  (integrate f local-a local-b local-n)))
    (let ((total-integral (mpi:mpi-reduce local-integral :MPI_SUM)))
      (mpi:formatp0 t "Integral is ~a~%" total-integral))))

(mpi:mpi-init) ;initialize MPI
(mpi:formatp0 t "Integrating (* x x)")
(trapezoidal-integration #'(lambda (x) (* x x)) 0 5 100)
(mpi:formatp0 t "Integrating with MPI_Reduce for collection")
(trapezoidal-integration-with-reduce #'(lambda (x) (* x x)) 0 5 100)
(mpi:mpi-finalize)
#+sbcl(sb-ext:quit)
#+cmu(ext:quit)
