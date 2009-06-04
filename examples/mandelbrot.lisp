;;;
;;; mandelbrot.lisp
;;;
;;; CL-MPI demo program to compute Mandelbrot set and outputs a image file
;;; Copyright 2009, Alex Fukunaga and Hidetoki Tanaka
;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'asdf)
  ;(format t "loading cl-mpi~%")
  (asdf:operate 'asdf:load-op 'cl-mpi))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf mpi:*enable-mpi* t))

(deftype mpi-rank-type () `(integer 0 10000))

(defun plot (pixel-x pixel-y max-iterations)
  "compute one pixel value"
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (integer 0 65535) pixel-x pixel-y max-iterations))
  (loop with xc double-float = (coerce pixel-x 'double-float) ;(coerce (/ pixel-x 1000) 'double-float)
	with yc double-float = (coerce pixel-y 'double-float) ;(coerce (/ pixel-y 1000) 'double-float)
	for x double-float = 0d0 then (+ xc (- (* x x) (* y y)))
	and y double-float = 0d0 then (+ yc (* 2d0 x y))
	for count fixnum = 0 then (1+ count)
	for temp = (+ (* x x) (* y y))
	when (>= temp 4d0) do (return-from plot count)
	while (and (<= temp  4d0) (< count max-iterations)))
  0)


(defun mandel (max-iteration max-x max-y &key (outputfile "mandel.pbm"))
  (declare (optimize (speed 3)(safety 3))
	   (fixnum max-iteration)
	   (type (integer 0 16384) max-x max-y))
  (mpi:with-mpi
      (let* ((start-time (mpi:mpi-wtime))
	     (image (make-array max-x 
				:element-type '(simple-array fixnum *)
				:initial-contents
				(loop repeat max-x collect
				      (make-array max-y :element-type 'fixnum 
							  :initial-element 0))))
	     (num-procs (mpi:mpi-comm-size))
	     (my-rank (mpi:mpi-comm-rank))
	     (start-x (floor (* my-rank max-x) num-procs))
	     (end-x (1- (the fixnum (floor (* (1+ my-rank) max-x) num-procs)))))
	(declare (type mpi-rank-type my-rank)
		 (fixnum num-procs start-x end-x))
	(when (and (= my-rank (1- num-procs)) (> end-x max-x))
	  (setf end-x max-x))

	;; compute local values
	(loop for i from start-x to end-x do
	      (loop for j from 0 to (1- max-y) do
		    (setf (aref (aref image i) j) (plot i j max-iteration))))
	(mpi:formatp t "Completed work in range(~a, ~a)~%" my-rank num-procs start-x end-x)
	(format t "bogus~%")(force-output t)
	
	;; collect image at rank 0
	(cond ((= my-rank 0)
	       (loop for rank of-type (integer 0 128) from 1 below (mpi:mpi-comm-size)
		     for recv-start-x fixnum = (floor (* rank max-x) num-procs)
		     for recv-end-x fixnum = (1- (the fixnum (floor (* (1+ rank) max-x) 
								    num-procs))) do
		     (when (and (= rank (1- num-procs))(> recv-end-x max-x))
		       (setf recv-end-x max-x))
		     (loop for i from recv-start-x to recv-end-x do
			   ;;(setf (aref image i) (mpi:mpi-receive-auto rank :tag 0))
			   (setf (aref image i) (mpi:mpi-receive rank :MPI_INT))
			   ;;(mpi:formatp t "recieved image[~a] from ~a.~%" i rank)
			   )))
	      (t
	       (loop for i from start-x to end-x do
		     (mpi:mpi-send-auto (aref image i) 0 :tag 0))))
	
	(when (= my-rank 0) ;; output a pbm image file
	  #+nil
	  (with-open-file (f outputfile :direction :output :if-exists :overwrite
			     :if-does-not-exist :create)
	    (declare (stream f))
	    (format f "P1~%~a ~a~%" max-x max-y) ; pbm format header
	    (loop for i from 0 to (1- max-x) do
		  (loop for j from 0 to (1- max-y) 
			for pix fixnum = (aref (aref image i) j) do
			(format f "~3d " (if (> pix 0) 1 0)))
		  (format f "~%")))
	  (format t "Runtime = ~a~%" (- (mpi:mpi-wtime) start-time))))))


