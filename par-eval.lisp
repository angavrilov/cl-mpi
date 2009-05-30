#|



 par-eval
 
 An extension for cl-mpi which provides a simple master-slave abstraction


 ** This is an experimental extension, not part of the "core" of CL-MPI.


Copyright (c) 2008,2009  Alex Fukunaga

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


|#



(defpackage #:par-eval
  (:documentation "CL-MPI library: Common Lisp bindings for the Message Passing Interface (MPI)")
  (:use #:mpi #:cl)
  (:export par-eval 
	   par-eval-seq 
	   slave-server
	   slave-server-1-shot
	   spawn-1-shot-evaluation
	   kill-slaves
  ))

(in-package #:par-eval)

(defparameter *trace-pe* nil "toggle tracing for par-eval")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slave-server-1-shot ()
  "a one-shot slave server"
  (let ((command (mpi-receive-string 0))) ;wait for a request
    (tracep *trace-pe* t "slave-server-1-shot: received command: ~a~%" command)
    (let ((result (eval (read-from-string command))))
      (tracep *trace-pe* t "slave-server-1-shot: computed result ~a" result)
      (mpi-send-string (write-to-string result) 0))))
      

(defun spawn-1-shot-evaluation (expr id)
  "evaluate expr somewhere"
  (mpi-send-string (write-to-string expr) id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct smessage
  "message to a slave-server"
  (command nil) ;one of: 'eval 'quit
  (payload nil)) ; if command=='eval, then this it the sexpr to eval

(defun slave-server ()
  (loop ;repeat ;1 do
   (let ((msg-from-master (read-from-string (mpi-receive-string 0)))) ;wait for a request
     (let ((*print-pretty* nil))
       (tracep *trace-pe* t "received message from master: ~a~%" msg-from-master))
     (ecase (smessage-command msg-from-master)
       (eval
	(let ((result (eval (smessage-payload msg-from-master))))
	  ;;(formatp t "computed result ~a" result)
	  (mpi-send-string (write-to-string result) 0)))
       (quit
	(return-from slave-server t))))))

(defun spawn-task (expr id)
  "evaluate expr somewhere"
   ;; XXX Maybe instead of write-so-string, this should be prin1-to-string??
  (mpi-send-string (write-to-string (make-smessage :command 'eval :payload expr)) id))
   

(defun par-eval-seq (expressions)
  "SINGLE-PROCESSOR VERSION of par-eval FOR DEBUGGING:
   Evaluate array of expressions #(expr1 expr2 .... exprn) in parallel. 
   returns array of results#(val1 val2 ... valn)
   Returns when all of the expressions are finished."
  (make-array (length expressions) :initial-contents
	      (loop for exp in expressions collect (eval exp))))

(defstruct job
  (id nil)
  (expr nil) ; the s-expression to evaluate on the slave
  (proc nil) ;the proc to which this job is assigned
  (status nil))

(defun run-job-on-proc (job proc-id proc-to-job-map)
  "assign job to processor with id proc-id, spawn the process."
  (spawn-task (job-expr job) proc-id)
  (setf (job-status job) 'running)
  (setf (job-proc job) proc-id)
  (setf (aref proc-to-job-map proc-id) job))

(defun par-eval (expressions)
  "Evaluate array of expressions #(expr1 expr2 .... exprn) in parallel. 
   returns array of results#(val1 val2 ... valn)
   Returns when all of the expressions are finished.
   This version does not do any load-balancing.
   This is not fault tolerant if a slave fails
   "
  ;(assert (<= (length expressions) (mpi-comm-size)))
  (let* ((num-jobs (length expressions))
	 (results (make-array num-jobs :initial-element nil)); result[i] = result of i'th expression
	 (proc-to-job-map (make-array num-jobs :initial-element nil)) ; proc-to-job[p] = id of job running on proc p
	 ;; all jobs are initially pending
	 (pending-jobs (loop for expr in expressions 
			     for index = 0 then (1+ index) collect
			     (make-job :id index :expr expr :status 'pending)))
	 (running-jobs nil))
    ;(format t "expressions=~a~%" expressions)
    ;(format t "~%Generated jobs: ~a~%" pending-jobs)

    ;; assign first N jobs to processors
    (loop for proc from 1 below (min num-jobs (mpi-comm-size) )
	  for job = (pop pending-jobs) do
	  (assert job)
	  (run-job-on-proc job proc proc-to-job-map)
	  (push job running-jobs))
	  
    (loop while running-jobs do ; while there are jobs which haven't returned results
	  ;; block until I receive a message from some child
	  #+nil(when (= 1 (length running-jobs))
		 (format t "Waiting for job: ~a~%" (first running-jobs)))
	  (let* ((status (mpi-probe MPI_ANY_SOURCE MPI_ANY_TAG :blocking t) )
		 (proc (status-source status)))
	    ;;multiple-value-bind (msg-length proc tag error)
	    ;; Store the result
	    (let ((job (aref proc-to-job-map proc))
		  (result (read-from-string (mpi-receive-string proc))))
	      (if (not job) (error "null job1!"))
	      (setf (aref results (job-id job)) result)
	      (setf (job-status job) 'completed)
	      (setf running-jobs (remove job running-jobs))
	      (tracep *trace-pe* t "received result from ~a: ~a~%" proc result)

	      ;; this  processor is now available
	      ;(assert (= 1 (count proc running-jobs :test #'(lambda (job) (= proc (job-proc job)))))) ; I should be expecting a message fromthis child

	      (when pending-jobs
		;; assign a new task to the newly freed processor
		(let ((next-job (pop pending-jobs)))
		  (run-job-on-proc next-job proc proc-to-job-map)
		  (push next-job running-jobs)))))
	  ;;(format t "At end of cycle, running-jobs=~a~%" running-jobs)
	  )
    ;; at this point, the master has collected all results from the slaves
    (tracep *trace-pe* t "collected results: ~a~%" results)
    results))

(defun kill-slaves ()
  (loop for i from 1 below (mpi-comm-size) do
	(mpi-send-string (write-to-string (make-smessage :command 'quit)) i)))


