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

(defun plot (pixel-x pixel-y n)
  "compute one pixel value"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(declare (optimize (speed 0) (safety 3) (debug 3)))
  (declare (type (integer 0 65535) pixel-x pixel-y n))
  (loop with x0 single-float = (float (/ pixel-x 1000))
	with y0 single-float = (float (/ pixel-y 1000))
	for x single-float = 0.0 then (+ x0 (- (* x x) (* y y)))
	and y single-float = 0.0 then (+ y0 (* 2 x y))
	for color fixnum = 0 then (1+ color)
	while (and (<= (+ (* x x) (* y y)) (* 2 2))
		   (< color n))
	finally  (return-from plot (if (= color n) 0 color))))

(defun mandel (max-iteration scale-x scale-y &key (outputfile "mandel.pbm"))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(declare (optimize (debug 3)(safety 0)(speed 0)))
  (declare (fixnum max-iteration)
	   (type (integer 0 16384) scale-x scale-y))

  (mpi:mpi-init)
  (let* ((start-time (mpi:mpi-wtime))
	 (image (make-array scale-x :element-type '(simple-array fixnum *) :initial-contents 
			    (loop repeat scale-x collect
				  (make-array scale-y :element-type 'fixnum 
						      :initial-element 0))))
	 (num-procs (mpi:mpi-comm-size))
	 (my-rank (mpi:mpi-comm-rank))
	 (start-x (floor (* my-rank scale-x) num-procs))
	 (end-x (1- (the fixnum (floor (* (1+ my-rank) scale-x) num-procs))))
	 (tag 0))
    (declare (type mpi-rank-type my-rank)
	     (fixnum num-procs start-x end-x tag))

    (when (and (= my-rank (1- num-procs)) (> end-x scale-x))
      (setf end-x scale-x))
    (when (= my-rank 0)
      (format t "max iteration = ~a, scale-x = ~a, scale-y = ~a~%" max-iteration scale-x scale-y))
    
    ;; compute local values
    (loop for i from start-x to end-x do
	  (loop for j from 0 to (1- scale-y) do
		(setf (svref (aref image i) j) (plot i j max-iteration))))
    (mpi:formatp t "I am ~a of ~a. My work is in range(~a, ~a)~%" my-rank num-procs start-x end-x)
    
    ;; collect image at rank 0
    (loop for comm-rank of-type mpi-rank-type from 1 to (1- num-procs) do
	  (when (= my-rank 0)
	    (let ((recv-start-x (floor (* comm-rank scale-x) num-procs))
		  (recv-end-x   (1- (the fixnum (floor (* (1+ comm-rank) scale-x) 
						       num-procs)))))
	      (declare (fixnum recv-start-x recv-end-x))
	      
	      (when (and (= comm-rank (1- num-procs))(> recv-end-x scale-x))
		(setf recv-end-x scale-x))
	      ;; (formatp t "recieving range(~a, ~a)~%" recv-start-x recv-end-x)
	      
		(loop for i from recv-start-x to recv-end-x do
		      (setf (aref image i)
			    #+nil(mpi:mpi-receive comm-rank '(simple-array fixnum *) scale-y :tag tag)
			    (mpi:mpi-receive-auto comm-rank :tag tag))
		      (mpi:formatp t "recieved image[~a] from ~a.~%" i comm-rank)
		      )))
	  (when (= my-rank comm-rank)
	      (loop for i from start-x to end-x do
		    ;;(formatp t "sending ~a length array" (length (aref image i)))
		    (mpi:mpi-send-auto (aref image i) 0 :tag tag)
		    ;;(formatp t "sending image[~a] to 0.~%" i)
		    )))

    (when (= my-rank 0) ;; output a pbm image file
      (with-open-file (f outputfile :direction :output :if-exists :overwrite
			 :if-does-not-exist :create)
	(declare (stream f))
	(format f "P1~%~a ~a~%" scale-x scale-y)
	(loop for i from 0 to (1- scale-x) do
	      (loop for j from 0 to (1- scale-y) 
		    for pix fixnum = (svref (aref image i) j) do
		    (format f "~3d " (if (> pix 0) 1 pix)))
		(format f "~%")))
      (format t "Running Time = ~a~%" (- (mpi:mpi-wtime) start-time)))

    (mpi:mpi-finalize)))


