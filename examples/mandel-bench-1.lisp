#|
CL-MPI Mandelbrot benchmark 
a parallelized version of Nikodemus Siivola's sequential version  posted 6/2/2009 at:
     http://random-state.net/log/3452921796.html
Alex Fukunaga 2009.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'asdf)
  ;(format t "loading cl-mpi~%")
  (asdf:operate 'asdf:load-op 'cl-mpi))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf mpi:*enable-mpi* t))


(declaim (optimize (speed 3) (debug 0)(safety 0)))

(defconstant +resolution+ 5000)
(defconstant +iterations+ 100)

(declaim (inline iters))

(defun iters (max xc yc)
  (declare (double-float xc yc))
  (let ((x xc)
        (y yc))
    (declare (double-float x y)
             (fixnum max))
    (loop for count from 0 below max
          do (when (>= (+ (* x x) (* y y)) 4.0d0)
               (return-from iters count))
             (let ((tmp (+ (- (* x x) (* y y)) xc)))
               (setf y (+ (* 2.0d0 x y) yc)
                     x tmp)))
    max))

(defun main ()
  "straightforward parallelization which partitions the work into equal regions,
   and each region is assigned to a process."
  (let* ((max (truncate +resolution+ 2))
         (min (- max))
         (mul (/ 2.0d0 max))
         (local-count 0))
    (declare (fixnum local-count))
    (mpi:with-mpi
      (let* ((start-time (mpi:mpi-wtime))
	     (num-procs (mpi:mpi-comm-size))
	     (my-rank (mpi:mpi-comm-rank))
	     (rows-per-proc (/ +resolution+ num-procs))
	     (local-min (+ min (* my-rank rows-per-proc)))
	     (local-max (1- (+ local-min rows-per-proc))))
	(declare (fixnum local-min local-max my-rank num-procs))
	(mpi:formatp t "local-min=~a, local-max=~a~%" local-min local-max)
	(loop for i from local-min upto local-max
	      do (loop for j from min upto max
		       do (incf local-count (iters +iterations+ (* mul i) (* mul j)))))
	(mpi:formatp t "local-count = ~d computed after time ~,4f seconds~%" 
		     local-count (- (mpi:mpi-wtime) start-time))
	;; aggregate local counts at rank 0
	(cond ((> my-rank 0) ; send local result to rank 0
	       (mpi:mpi-send-auto local-count 0))
	      (t ;my-rank is  0. Receive and aggregate local results.
	       (loop with count fixnum = local-count
		     for rank from 1 below num-procs
		     do (incf count (mpi:mpi-receive-auto rank))
		     finally (format t "runtime=~,4f seconds, result ~d~%" 
				      (- (mpi:mpi-wtime) start-time) count))))))))

#+nil
(save-lisp-and-die "mandel" :executable t :toplevel #'main)
