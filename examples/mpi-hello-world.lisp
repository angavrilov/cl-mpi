#+sbcl(require 'asdf)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cl-mpi))

(defun mpi-hello-world ()
  "Each process other than the root (rank 0) sends a message to process 0"
  (mpi::with-mpi  ;macro which initializes and finalizes  MPI
      (let ((tag 0))
	(cond ((/= 0 (mpi:mpi-comm-rank))
	       (let ((msg (format nil "Greetings from process ~a!" (mpi:mpi-comm-rank))))
		 ;;send msg to proc 0
		 (mpi:mpi-send-auto msg 0 :tag tag)))
	      (t ; (mpi-comm-rank) is 0
	       (loop for source from 1 below (mpi:mpi-comm-size) do
		     ;; receive and print message from each processor
		     (let ((message (mpi:mpi-receive-auto  source :tag tag)))
		       (format t "~a~%" message))))))
    ))

