#|

mpi-test.lisp

Test suite for CL-MPI

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

(in-package #:mpi)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some Tests
;;;


(defparameter *test-failures* nil)

(defmacro testassert (body)
  "This must be in every test"
  (let ((g-res (gensym)))
    `(let ((,g-res ,body))
       (when (not ,g-res)
	 (formatp t "TEST FAILURE: ~a" (quote ,body))
	 (push (list mpi-test-title (quote ,body))*test-failures*);; add the title of the failed test
	 )
       ,g-res)))

(defmacro with-test-harness ((&key (title nil)) &body body)
  (let ((header (when title
		  `((formatp0 t "~%")(formatp0 t "=======================~% *** ~a~%" ,title))))
	(g-result (gensym)))
    `(with-buffer 1000
       ,@header
       (let* ((mpi-test-title ,title) ; intentional capture of mpi-test-title, to be used by testassert
	      (,g-result  ;save the result of the body to g-result
	       (progn
		 ,@body)))
	 ;;(declare (ignore mpi-test-title)) ;;XXX ??? here to suppress warnings, but is this safe?
	 (force-output t)
	 (mpi-barrier)  ; clean up
      ,g-result)))) ; return the result of the body


(defun test-testassert ()
  "This is to test the testassert test failure detection and collection mechanism.
   It INTENTIONALLY generates an error."
  (with-test-harness (:title "test testassert - Generate an INTENTIONAL error. Don*t worry, this is not a real failure!")
    (let ((bogo 2))
      (testassert (= bogo 3.14159)))))

(defun test-blocking-spawn ()
  (with-test-harness (:title "Test blocking spawn 1")
    (cond ((= 0 (mpi-comm-rank))
	   (loop for i from 1 below (mpi-comm-size) do
		 (par-eval:spawn-1-shot-evaluation `(* ,i ,i) i))
	   ;; collect results ; note that mpi-receive-string blocks, so it means that the loop can't complete until
	   ;; all results have been computed.
	   (loop for i from 1 below (mpi-comm-size) do
		 (let ((result (mpi-receive-string i)))
		   (formatp t "Result from ~a: ~a~%" i result)
		   (testassert (= (* i i) (read-from-string result)))
		   )))
	  (t ; slaves
	   (par-eval:slave-server-1-shot))))

  (with-test-harness (:title "Test blocking spawn 2")
    (cond ((= 0 (mpi-comm-rank))
	   (loop for i from 1 below (mpi-comm-size) do
		 (par-eval:spawn-1-shot-evaluation `(* ,(coerce i 'single-float) ,(coerce i 'single-float)) i))
	   ;; collect results ; note that mpi-receive-string blocks, so it means that the loop can't complete until
	   ;; all results have been computed.
	   (loop for i from 1 below (mpi-comm-size) do
		 (let ((result (mpi-receive-string i)))
		   (formatp t "Result from ~a: ~a~%" i result)
		   (testassert (= (coerce (* i i)'single-float) (read-from-string result)))
		   )))
	  (t ; slaves
	   (par-eval:slave-server-1-shot))))

  (with-test-harness (:title "Test blocking spawn 3")
    (cond ((= 0 (mpi-comm-rank))
	   (loop for i from 1 below (mpi-comm-size) do
		 (par-eval:spawn-1-shot-evaluation `(* ,(coerce i 'double-float) ,(coerce i 'double-float)) i))
	   ;; collect results ; note that mpi-receive-string blocks, so it means that the loop can't complete until
	   ;; all results have been computed.
	   (loop for i from 1 below (mpi-comm-size) do
		 (let ((result (mpi-receive-string i)))
		   (formatp t "Result from ~a: ~a~%" i result)
		   (testassert (= (coerce (* i i)'double-float) (read-from-string result)))
		   )))
	  (t ; slaves
	   (par-eval:slave-server-1-shot))))
  )

(defun test-blocking-spawn2 ()
  (with-test-harness (:title "Test blocking spawn 2")
    (cond ((= 0 (mpi-comm-rank))
	   (let* ((active nil))
	     (loop for i from 1 below (mpi-comm-size) do
		   (push i active)
		   (par-eval:spawn-1-shot-evaluation `(* ,i ,i) i))
	     ;; now collect the results
	     (loop while active do ; while there are jobs which haven't returned results
		   ;; block until I receive a message from some child
		   (let* ((status (mpi-probe MPI_ANY_SOURCE MPI_ANY_TAG :blocking t))
			  (source (status-source status)))
		     ;;(multiple-value-bind (count source tag error)
		     (assert (= 1 (count source active)))
		     (setf active (remove source active))
		     (let ((result (mpi-receive-string source)))
		       (formatp t "received result from ~a: ~a, still waiting for ~a ~%" source result active)
		       (testassert (= (* source source) (read-from-string result))))
		     ))))
	  (t ; slaves
	   (par-eval:slave-server-1-shot)))))

(defun test-par-eval1 ()
  (with-test-harness (:title "test par-eval")
    (cond ((= 0 (mpi-comm-rank))
	   (let ((result (par-eval:par-eval (loop for i from 0 below (* 3 (mpi-comm-size)) collect
						  `(* ,i ,i))))
		 (expected-result  (loop for i from 0 below (* 3 (mpi-comm-size)) collect
					 (* i i))))
	     (testassert (equalp (coerce result 'list) expected-result)))
	   (par-eval:kill-slaves))
	  (t
	   (par-eval:slave-server))))

  (with-test-harness (:title "test par-eval 2")
    (cond ((= 0 (mpi-comm-rank))
	   (let ((result (par-eval:par-eval (loop for i from 0 below (- (mpi-comm-size) 1) collect
						  `(* ,i ,i))))
		 (expected-result  (loop for i from 0 below (- (mpi-comm-size) 1) collect
					 (* i i))))
	     (testassert (equalp (coerce result 'list) expected-result)))
	   (par-eval:kill-slaves))
	  (t
	   (par-eval:slave-server))))
  )

(defun my-next-neighbor ()
  "gives next neighbor (toroidal)"
  (if (< (mpi-comm-rank) (1- (mpi-comm-size)))
      (1+ (mpi-comm-rank))
      0))

(defun my-prev-neighbor ()
  "gives prev neighbor (toroidal)"
  (if (> (mpi-comm-rank) 0)
      (1- (mpi-comm-rank))
      (1- (mpi-comm-size))))

(defun test-send-receive ()
  (with-test-harness (:title "test send-receive")
    (multiple-value-bind (received-str size)
	(mpi-send-receive-string (write-to-string (mpi-comm-rank))
				 (my-next-neighbor) (my-prev-neighbor))
      (formatp t "received ~a [~a] from neighbor~%" received-str size)
      (testassert (= (read-from-string received-str) (my-prev-neighbor))))))

(defun test-send-and-receive (data &key (root 0))
  "XXX handle synchrnous, buffered modes as well?"
  (with-test-harness (:title (format nil "test send and receive ~a" data))
    (cond ((= root (mpi-comm-rank))
	   (loop for i from 0 below (mpi-comm-size)
		 when (/= i root) do
		 (mpi-send data i)))
	  (t ; (/= root (mpi-comm-rank))
	   ;;(let ((received (mpi-receive root (type-of data) (if (arrayp data) (length data) 1))))
	   (formatp t "expecting to receive object of type ~a" (type-of data))
	   (let ((received (mpi-receive1 root (type-of data) (if (arrayp data) (length data) 1))))
	     (formatp t "received  ~a~%" received)
	     (testassert (equalp received data)))))))

(defun test-send-receive-types ()
  "Test whether I can send messages between processors regular"
  ;; blocking send/receive
  (dolist (mode '(:basic :synchronous :buffered))
    (declare (ignore mode)) ;;XXX TEMP!
    (let ((*print-pretty* nil)
	  (objects-to-send `(0 -127  ;char
			     128 255 ;unsigned char
			     256 1.1f0 2.2d0
			     ,(make-array 3 :element-type 'fixnum :initial-contents '(-1 -2 -3))
			     ,(make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))
			     ,(make-array 3 :element-type 'single-float :initial-contents '(1.1 2.2 3.3))
			     ,(make-array 3 :element-type 'double-float :initial-contents '(1.1d0 2.2d0 3.3d0))
			     )))
      (loop for obj in objects-to-send do
	    (test-send-and-receive obj)))))


(defun meaningless-computation (x)
  "Let's waste some time"
  (let ((sum 0))
    (loop for i from 0 below 100000 do
	  (incf sum (+ i (loop for i from 0 below x summing (* x x x x)))))
    sum))

(defparameter *test-msg1* "01234567890123456789")

(defun test-wait-all ()
  (with-test-harness (:title "test-wait-all")
    (cond ((= 0 (mpi-comm-rank))
	   (let ((requests
		  (loop for r from 1 below (mpi-comm-size) collect
			;(mpi-send-string *test-msg1* r :tag 1000 :blocking nil :mode :synchronous))))
			(mpi-send-string *test-msg1* r :tag 1000 :blocking nil))))
	     (sleep 1)
	     (let ((statuses
		    (mpi-wait-all (make-array (1- (mpi-comm-size)) :initial-contents requests))))
	       (formatp t "mpi-wait-all returned  at ~a~%" (mpi-wtime))
	       (formatp t "statuses=~a~%" statuses))))
	  (t
	   (let ((foo (meaningless-computation 1234)))
	     (formatp t "meaningless computation: ~a~%" foo))
	   (sleep 1.0)
	   (multiple-value-bind (msg size)
	       (mpi-receive-string 0 :tag 1000)
	     (formatp t "msg [size=~a,leng=~d] ~a received at ~a~%"  size (length msg) msg (mpi-wtime))
	     (testassert (string-equal msg *test-msg1*)))))))

(defun test-wait-any2 ()
  "nonblocking receive, processed with wait-any2"
  (with-test-harness (:title "Test wait-any")
    (cond ((= 0 (mpi-comm-rank)) ;nonblocking receive a bunch of messages
	   (let ((*print-pretty* nil)
		 (requests (loop for r from 1 below (mpi-comm-size) collect
				 (mpi-receive-string-nonblocking r :tag 1000 :buf-size-bytes 20))))
	     (loop while requests do
		   (multiple-value-bind (completed-request status remaining-requests)
		     (mpi-wait-any2 requests)
		     (formatp t "Non-blocking receive request ~a completed with status ~a~%"
			      completed-request status)
		     (let ((msg (request-get-string completed-request (status-count status))))
		       (formatp t "msg= ~a~%" msg)
		       (testassert (string-equal msg *test-msg1*)))
		     (setf requests remaining-requests)))))
	  (t
	   (sleep (random 0.5))
	   (mpi-send-string *test-msg1* 0 :tag 1000 :blocking t :mode  :basic)))))

(defun test-test-any-with-delay (delay)
  (with-test-harness (:title "test-test-any.some message arrives or not because of delay?")
    (cond ((= 0 (mpi-comm-rank))
	   (let ((requests (loop for r from 1 below (mpi-comm-size) collect
				 (mpi-receive-string-nonblocking r :tag 1000))))
	     (sleep 0.5)
	     (multiple-value-bind (out-index status)
		 (mpi-test-any requests)
	       (testassert t);;???
	       (formatp t "test-any with delay ~a result: out-index=~a, status=~a~%"
			delay out-index status))))
	  (t
	   (sleep delay)
	   (mpi-send-string *test-msg1* 0 :tag 1000 :blocking t :mode  :basic)))))

(defun test-test-all ()
  (with-test-harness (:title "test-test-all. loop until all messages arrive")
    (cond ((= 0 (mpi-comm-rank))
	   (let ((requests (loop for r from 1 below (mpi-comm-size) collect
				 (mpi-receive-string-nonblocking r :tag 1000))))
	     (loop
	      (multiple-value-bind (status)
		  (mpi-test-all requests)
		(when status
		  (formatp t "received ~a~%" status)
		  (return))))))
	  (t
	   (sleep (+ (coerce (mpi-comm-rank) 'float)(random 0.5)))
	   (mpi-send-string *test-msg1* 0 :tag 1000 :blocking t :mode :basic)))
    (testassert t))) ;XXX obligatory

(defun test-test ()
  (with-test-harness (:title "test-test. loop until all messages arrive")
    (cond ((= 0 (mpi-comm-rank))
	   (let ((requests (loop for r from 1 below (mpi-comm-size) collect
				 (mpi-receive-string-nonblocking r :tag 1000))))
	     (loop while requests do
		   (multiple-value-bind (status)
		       (mpi-test (first requests))
		     (when status
		       (formatp t "received ~a~%" status)
		       (setf requests (rest requests)))))))
	  (t
	   (sleep (+ (coerce (mpi-comm-rank) 'float)(random 0.5)))
	   (mpi-send-string *test-msg1* 0 :tag 1000 :blocking t :mode :basic)))
    (testassert t))) ;XXX obligatory


(defun test-wait/test-some2 (mode)
  "nonblocking receive, processed with wait-some2"
  (with-test-harness (:title "test-wait-some2")
    (format t "mode=~a~%" mode)
    (cond ((= 0 (mpi-comm-rank)) ;nonblocking receive a bunch of messages
	   (let ((*print-pretty* nil)
		 (requests (loop for r from 1 below (mpi-comm-size) collect
				 (mpi-receive-string-nonblocking r :tag 1000))))
	     (print requests)
	     (loop while requests do
		   (multiple-value-bind (completed-requests remaining-requests)
		       (case mode
			 (:wait (mpi-wait-some2 requests))
			 (:test (mpi-test-some2 requests)))
		     (when (case mode
			     (:wait t)
			     (:test completed-requests))
		       (formatp t "Completed receives:~%~a~%remaining=~a~%" completed-requests remaining-requests)
		       (loop for req in completed-requests do
			     (let ((msg (request-get-string (car req) (status-count (cdr req)))))
			       (formatp t "msg= ~a~%" msg)
			       (testassert (string-equal msg *test-msg1*))))
		       (setf requests remaining-requests))))))
	  (t
	   (sleep (random 0.5))
	   (mpi-send-string *test-msg1* 0 :tag 1000 :blocking t :mode  :basic)))))

(defun test-nonblocking-probe (delay)
  (with-test-harness (:title "test-nonblocking-probe")
    (cond ((= 0 (mpi-comm-rank))
	     (sleep delay)
	   (loop for r from 1 below (mpi-comm-size) do
		 (mpi-send-string *test-msg1* r :tag 1000 :blocking nil :mode :basic)))
	  (t
	   (formatp t "Nonblocking probe result ~a~%" (mpi-probe MPI_ANY_SOURCE MPI_ANY_TAG :blocking nil))
	   (sleep 1.0) ; wait long enough that proc 0 has finished sending
	   (formatp t "Nonblocking probe result ~a~%" (mpi-probe MPI_ANY_SOURCE MPI_ANY_TAG :blocking nil))
	   (multiple-value-bind (msg size)
	       (mpi-receive-string 0 :tag 1000)
	     (formatp t "msg [size=~a,leng=~d] ~a received at ~a~%"  size (length msg) msg (mpi-wtime))
	     (testassert (string-equal msg *test-msg1*)))))))

(defun test-non-blocking-receive ()
  " * a 'test' call is actually a completion, can't call test/wait again if a call completes!"
  (with-test-harness (:title "test-non-blocking-receive")
    (cond ((= 0 (mpi-comm-rank))
	   (sleep 0.10000) ;;XXX VERY IMPORTANT????
	   (let ((sent (loop for r from 1 below (mpi-comm-size) collect
			     (mpi-send-string *test-msg1* r :tag 1000 :blocking nil :mode :synchronous))))
	     (loop for req in sent do (mpi-wait req))))
	  (t
	   (let ((req (mpi-receive-string-nonblocking 0 :tag 1000)))
	     (let ((status (mpi-wait req))
		   (*print-pretty* nil))
	       (formatp t "receive-nonblocking status = ~a~%" status)
	       (let ((msg (request-get-string req (status-count status))))
		 (formatp t "Nonblocking receive received ~a" msg)
		 (testassert (string-equal msg *test-msg1*))
		 )))))))

(defun test-non-blocking-send ()
  (with-test-harness (:title "test-non-blocking-send")
    (cond ((= 0 (mpi-comm-rank))
	   (loop for r from 1 below (mpi-comm-size)
		 for req = (mpi-send-string *test-msg1* r :tag 1000 :blocking nil :mode :basic) do
		 (formatp t "Nonblocking send sent to ~a at ~a~%" req (mpi-wtime))
		 (formatp t "Mpi-test on ~a, result=~a~%" r (mpi-test req))))
	  (t
	   (when (= 1 (mpi-comm-rank))
	     (formatp t "meaningless computation: ~a~%" (meaningless-computation 1234))
	     (sleep 1.0))
	   (multiple-value-bind (msg size)
	       (mpi-receive-string 0 :tag 1000)
	     (formatp t "msg [size=~a,leng=~d] ~a received at ~a~%"  size (length msg) msg (mpi-wtime))
	     (testassert (string-equal msg *test-msg1*))
	     )))))

(defun test-blocking-send-receive-simple ()
  "Test whether I can send messages between processors"
  ;; initialize message buffer (used only in buffered mode test
  (with-test-harness (:title "test-blocking-send-receive-simple")
    ;; blocking send/receive
    (dolist (mode '(:basic :synchronous :buffered))
      (cond ((= 0 (mpi-comm-rank))
	     (loop for i from 1 below (mpi-comm-size) do
		   (mpi-send-string (format nil "Master to proc #~a: Hello!" i) i :mode mode))
	     (loop for i from 1 below (mpi-comm-size) do
		   ;;master blocks on each processor until responses are received
		   (formatp t "received reply from ~a: ~a~%" i (mpi-receive-string i))))
	    (t ; (/= 0 (mpi-comm-rank)  code for slaves
	     (let ((inbox (mpi-receive-string 0))) ;block until message received from master
	       (formatp t "received message ~a~%" inbox)
	       (mpi-send-string (format nil "Proc #~a reporting for duty!" (mpi-comm-rank)) 0 :mode mode))))
      (formatp t "ready to barrier")
      (mpi-barrier)
      (formatp t "after the barrier")
      (testassert t);obligatory
      (formatp0 t "Passed ~a mode test of blocking send/receive of strings~%" mode)))

  (with-test-harness (:title "test blocking probe");; test blocking probe
    (cond ((= 0 (mpi-comm-rank))
	   (loop for i from 1 below (mpi-comm-size) do
		 (mpi-send-string (format nil "Rank 0 to Rank ~a probe test.~%" i) i :tag 1234)))
	  (t
	   (let ((status (mpi-probe MPI_ANY_SOURCE MPI_ANY_TAG :blocking t :base-type 'character)))
	     (formatp t "probe detected message from ~a, count=~a, tag=~a, error=~a"
		      (status-source status)(status-count status)(status-tag status)(status-error status))
	     (formatp t "received message: ~a" (mpi-receive-string (status-source status)
								   :buf-size-bytes (status-count status)
								   :tag (status-tag status))))))
    (force-output t)
    (testassert t)
    ))

(defun test-broadcast ()
  (with-test-harness (:title "test-broadcast")
    (let ((r (mpi-broadcast 3145)))
      (formatp t "Bcast received ~a~%" r))
    (let ((r (mpi-broadcast 3.14)))
      (formatp t "Bcast received ~a~%" r))
    (let ((r (mpi-broadcast 3.14d0)))
      (formatp t "Bcast received ~a~%" r))
    (let ((r (mpi-broadcast (make-array 3 :element-type '(unsigned-byte 32) :initial-contents '(1 2 3)))))
      (formatp t "Bcast received ~a~%" r))
    (let ((r (mpi-broadcast "abcde")))
      (formatp t "Bcast received ~a~%" r))
    (testassert t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct bogostruct
  (a 0 :type fixnum)
  (b #\a :type character))


(defun test-broadcast-auto-object (object description-string &key (root 0))
  "Tests the broadcast-auto mechanism.
   Uses mpi-broadcast-auto to sends a message from the root to every proc with rank >= 1
   Each process then reflects the message to the root, where it is compared with the sent message."

  (with-test-harness (:title description-string)
    (let ((received-object nil) ; the messages received by this processor (only used for rank 0)
	  (sent-object nil)) ; the message which is sent by the root
      (when (= root (mpi-comm-rank))
	(setf sent-object object))
      (setf received-object (mpi-broadcast-auto sent-object )) ;;check 1
      (formatp t "Received broadcast object = ~a~%" received-object)(force-output t)
      (mpi-barrier)(force-output t)

      ;; now, reflect the messages back to the root compare, and verify that the round trip succeeded
      (cond ((/= root (mpi-comm-rank))
	     (mpi-send-auto received-object root));;check 2
	    (t ; (mpi-comm-rank) = root proc
	     (loop for s from 0 below (mpi-comm-size) do
		   (when (/= s root)
		     (let ((reflected-object (mpi-receive-auto s)))
		       (testassert (equalp sent-object reflected-object));NOTE: equalp performs a particular type of array equality check - see CLHS
		       (when (not (equalp sent-object reflected-object))
			 (formatp t "Problem: sent=~a, reflected=~a~%" sent-object reflected-object)
			 (formatp t "type-of sent=~a, type-of received=~a" (type-of sent-object) (type-of reflected-object))
			 (formatp t "length of sent=~a, length of received=~a" (length sent-object) (length reflected-object))
			 )))))))))


(defun test-send-auto-object (object description-string &key (root 0))
  "Tests the send-auto/receive mechanism.
   Uses mpi-send-auto to sends a message from every proc with rank >= 1 to the root,
   which uses mpi-receive-auto to receive the message.
   The root stores these messages, and then reflects them back to the sending proc, using mpi-send-auto.
   The reflected message is receive-autod by the sending proc, and the sent and reflect messages are compared using equalp"
  (with-test-harness (:title description-string)
    (let ((received-objects (make-array (mpi-comm-size))) ; the messages received by this processor (only used for rank 0)
	  (sent-object nil)) ; the message that this processor sent
      (cond ((/= root (mpi-comm-rank)) ;;send msg to proc 0
	     (setf sent-object object)
	     (mpi-send-auto sent-object 0)) ;;1,2,3,3-1,3-2 in mpi-send-auto
	    (t ; (mpi-comm-rank) is root
	     (loop for source from 1 below (mpi-comm-size) do
		   ;; receive and print message from each processor
		   (let ((recvd (mpi-receive-auto  source))) ;;1,2,3,3-1,3-2 in mpi-receive-auto
		     (format t "~a~%" recvd)
		     (format t "Received type=~a~%" (type-of recvd))
		     (setf (aref received-objects source) recvd)))))
      (mpi-barrier)
      ;; now, reflect the messages back to the sources and compare, to verify that the round trip succeeded
      (cond ((= root (mpi-comm-rank))
	     (loop for s from 1 below (mpi-comm-size) do
		   (mpi-send-auto (aref received-objects s) s)))
	    (t ;non-root procs
	     (let ((reflected-object (mpi-receive-auto 0)))
	       (testassert (equalp sent-object reflected-object)) ; NOTE: equalp performs a particular type of array equality check - see CLHS
	       (when (not (equalp sent-object reflected-object))
		 (formatp t "Problem: sent-object=~a, reflected-object=~a~%" sent-object reflected-object)
		 (formatp t "type-of sent=~a, type-of received=~a" (type-of sent-object) (type-of reflected-object))
		 (formatp t "length of sent=~a, length of received=~a" (length sent-object) (length reflected-object))
		 )))))))

(defparameter *name* #("T" "unsigned int" "int" "single float" "double float" "character" "string" "simple array" "simple array double-float" "fraction"  "list" "struct"))

(defparameter *function* #(T
			   (mpi-comm-rank)
			   (* -1 (mpi-comm-rank))
			   (+ (mpi-comm-rank) 0.0)
			   (+ (mpi-comm-rank) 0.0d0)
			   ;;#\a <== fail! but next is OK
			   (coerce (format nil "~a" (mpi-comm-rank)) 'character)
			   (format nil "Greetings from process ~a!" (mpi-comm-rank))
			   (make-array 4 :element-type 'fixnum :initial-contents (loop for i from 0 below 4 collect (* i (mpi-comm-rank))))
			   (make-array 4 :element-type 'double-float :initial-contents (loop for i from 0 below 4 collect (+ 0.0d0(* i (mpi-comm-rank)))))
			   (/ (mpi-comm-rank)  (+ 1 (mpi-comm-rank)))

			   (list 'a 1 1/2 1d0 "a" (make-bogostruct :a (* 10 (mpi-comm-rank)) :b#\c))
			   (make-bogostruct :a 30 :b #\C) ))
(defparameter *func-num* 10)

(defun test-send-auto ()
  (loop for a from 0 below *func-num* do
	(test-send-auto-object (aref *function* a)(format nil "Test send auto a ~a" (aref *name* a))))
  )


(defun test-broadcast-auto ()
  (loop for a from 0 below *func-num* do
	(test-broadcast-auto-object (aref *function* a)(format nil "Test broadcast auto a ~a" (aref *name* a))))
  )

(defun test-mpi-reduce (&key(root 0)(allreduce nil))
  ;; at each processor, let data be (rank*10, rank*10+1,...rank*10+9
  (let ((data (make-array 10 :element-type 'fixnum :initial-contents
			  (loop for i from 0 below 10 collect (+ i (* (mpi-comm-rank) 10) ))))
	(data3 (make-array 10  :initial-contents
			   (loop for i from 0 below 10 collect (* (mpi-comm-rank)(sqrt (* -1 i))))))
	(data2 (make-array 10 :element-type 'double-float :initial-contents
			   (loop for i from 0 below 10 collect (+ i (* (mpi-comm-rank) 10d0) ))))

	(data4 (make-array 10 :element-type 'single-float :initial-contents
			   (loop for i from 0 below 10 collect (+ i (* (mpi-comm-rank) 10.0) )))))


    (with-test-harness (:title (format nil "test reduce MPI_MAX, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data :MPI_MAX :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(testassert (equal (aref r i) (+ i (*(- (mpi-comm-size) 1) 10))))))))

    (with-test-harness (:title (format nil "test reduce MPI_MAX_SINGLE_FLOAT, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data4 :MPI_MAX :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(testassert (= (aref r i) (+ i (*(- (mpi-comm-size) 1) 10.0))))))))

    (with-test-harness (:title (format nil "test reduce MPI_MAX_DOUBLE_FLOAT, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data2 :MPI_MAX :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(testassert (= (aref r i) (+ i (*(- (mpi-comm-size) 1) 10d0))))))))

    (with-test-harness (:title (format nil "test reduce MPI_MIN, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data :MPI_MIN :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(testassert (= (aref r i) i))))))

    (with-test-harness (:title (format nil "test reduce MPI_SUM, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data :MPI_SUM :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(testassert (= (aref r i)
			       (loop for p from 0 below (mpi-comm-size) summing (+ i (* p 10))))))
	  )))


    (with-test-harness (:title (format nil "test reduce MPI_SUM_REAL, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data2 :MPI_SUM :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(testassert (= (aref r i)
			       (loop for p from 0 below (mpi-comm-size) summing (+ i (* p 10d0))))))
	  )))
    (with-test-harness (:title (format nil "test reduce MPI_PROD, allreduce=~a" allreduce))
      (let ((r (mpi-reduce data :MPI_PROD :allreduce allreduce)))
	(formatp t "r = ~a~%" r)(force-output t)
	(when (or allreduce (= root (mpi-comm-rank)))
	  (loop for i from 0 below 10 do
		(let ((s 1))
		  (loop for p from 0 below (mpi-comm-size) do (setf s (* s (+ i (* p 10)))))
		  (testassert (= (aref r i) s)))
	  )))
    )))

(defun test-mpi-scatter ()
  (with-test-harness (:title "Test mpi-scatter with fixnum")
    (let ((data (make-array (* (mpi-comm-size) 3) :element-type 'fixnum :initial-contents
			    (loop for i from 0 below (* (mpi-comm-size) 3) collect  i))))
      (let ((r (mpi-scatter data)))
	(formatp t "r=~a" r)
	(testassert (equalp r (make-array 3  :element-type 'fixnum :initial-contents
					  (loop for i from 0 below 3 collect  (+ i (* 3 (mpi-comm-rank))))))
	))))
  (with-test-harness (:title "Test mpi-scatter with float")
    (let ((data (make-array (* (mpi-comm-size) 3)  :element-type 'single-float  :initial-contents
			    (loop for i from 0 below (* (mpi-comm-size) 3) collect (coerce i 'single-float)))))
      (let ((r (mpi-scatter data)))
	(formatp t "r=~a" r)
	(testassert (equalp r (make-array 3   :element-type 'single-float :initial-contents
					  (loop for i from 0 below 3 collect (coerce (+ i (* 3 (mpi-comm-rank))) 'single-float))))
	))))

;;fail!!
;; XXX REASON: we don't support characters as a standard base type
#+nil(with-test-harness (:title "Test mpi-scatter with character")
    (let ((data (make-array (* (mpi-comm-size) 3)  :element-type 'standard-char  :initial-contents
			    (loop for i from 0 below (* (mpi-comm-size) 3) collect (aref (format nil "~a" i) 0)))))
      (let ((r (mpi-scatter data)))
	(formatp t "r=~a" r)
	(testassert (equalp r (make-array 3   :element-type 'standard-char :initial-contents
					  (loop for i from 0 below 3 collect (aref (format nil "~a" (+ i (* 3 (mpi-comm-rank)))) 0))))
		    ))))
  )


(defun test-mpi-gather (&key(all nil)(root 0))
  (with-test-harness (:title (if all "test-mpi-allgather with fixnum" "test-mpi-gather with fixnum"))
    (let ((data (make-array 3 :element-type 'fixnum :initial-contents
			    (loop for i from 0 below 3 collect (+ i (* 3 (mpi-comm-rank)))))))
      (formatp t "data=~a" data)
      (let ((r (if all
		   (mpi-allgather data)
		   (mpi-gather data))))
	(formatp t "r=~a" r)
	(if (or (= root (mpi-comm-rank))
		all)
	    (testassert (equalp r (make-array (* 3 (mpi-comm-size)) :element-type 'fixnum :initial-contents
					      (loop for i from 0 below (* 3 (mpi-comm-size))
						    collect i)))))
	)))
  (with-test-harness (:title (if all "test-mpi-allgather with float" "test-mpi-gather with float"))
    (let ((data (make-array 3 :element-type 'single-float :initial-contents
			    (loop for i from 0 below 3 collect (coerce (+ i (* 3 (mpi-comm-rank))) 'single-float)))))
      (formatp t "data=~a" data)
      (let ((r (if all
		   (mpi-allgather data)
		   (mpi-gather data))))
	(formatp t "r=~a" r)
	(if (or (= root (mpi-comm-rank))
		all)
	    (testassert (equalp r (make-array (* 3 (mpi-comm-size)) :element-type 'single-float :initial-contents
					      (loop for i from 0 below (* 3 (mpi-comm-size))
						    collect (coerce i 'single-float))))))
	)))

  ;;fail!!
  ;; because we don't support characters as base type
  #+nil(with-test-harness (:title (if all "test-mpi-allgather with char" "test-mpi-gather with char"))
	 (let ((data (make-array 3 :element-type 'standard-char :initial-contents
				 (loop for i from 0 below 3 collect (aref (format nil "~a"(+ i (* 3 (mpi-comm-rank)))) 0)))))
	   (formatp t "data=~a" data)
	   (let ((r (if all
			(mpi-allgather data)
			(mpi-gather data))))
	     (formatp t "r=~a" r)
	     (if (or (= root (mpi-comm-rank))
		     all)
		 (testassert (equalp r (make-array (* 3 (mpi-comm-size)) :element-type 'single-float :initial-contents
						   (loop for i from 0 below (* 3 (mpi-comm-size))
							 collect (aref (format nil "~a"  i) 0))))))
	     )))

  ;;fail!!
  ;; because arrays are not basetypes
  #+nil(with-test-harness (:title (if all "test-mpi-allgather with array" "test-mpi-gather with array"))
	 (let ((data (make-array 3 :element-type 'simple-array :initial-contents
				 (loop for i from 0 below 3 collect (loop for j from 0 below 3 collect (coerce (+ i j (* 3 (mpi-comm-rank))) 'single-float)  )))))
	   (formatp t "data=~a" data)
	   (let ((r (if all
			(mpi-allgather data)
			(mpi-gather data))))
	     (formatp t "r=~a" r)
	     (if (or (= root (mpi-comm-rank))
		     all)
		 (testassert (equalp r (make-array (* 3 (mpi-comm-size)) :element-type 'simple-array :initial-contents
						   (loop for i from 0 below (* 3 (mpi-comm-size))
							 collect (loop for j from 0 below 3 collect (coerce (+ j i) 'single-float)))))))
	     )))
  )


(defun testmpi ()
  (load-mpi-foreign-libraries)
  ;; test mpi-initialized
  (let ((mpi::*trace1* t))
    (setf *test-failures* nil)
    (format t "before init: MPI_Initialized = ~a~%" (mpi-initialized))
    (assert (not (mpi-initialized)))
    (mpi-init)
    (formatp t "Started MPI. Processor Name=~a, MPI_Initialized=~a at ~a(Wtick=~a).~%"
	     (mpi-get-processor-name)(mpi-initialized)(mpi-wtime)(mpi-wtick))

    (assert (mpi-initialized))
    #+nil(formatp0 t "mpi-comm-size=~a~%" (mpi-comm-size))
    (format t "mpi-comm-size=~a~%" (mpi-comm-size))
    (mpi-barrier)
    (formatp t "Past mpi-barrier.~%")
    (force-output t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;(trace mpi-receive1)
    ;;(trace mpi-send-1)
    ;;(trace mpi-receive)
    ;;(trace mpi-send)
    ;;(trace mpi-probe)

   (test-testassert)
   (test-blocking-send-receive-simple) ;;check
   (test-send-receive-types);; ??error
   (test-wait-any2) ;;check
   (test-wait/test-some2 :wait) ;;check
   (test-wait/test-some2 :test) ;;check but program is wrong

   #-cmu (test-wait-all) ;;XXX TODO  broken on cmucl+mpich1.2for some reason..
   (test-test) ;;check

   (test-test-all) ;;check
   (test-test-any-with-delay 0.0) ;;check
   (test-test-any-with-delay 1.0) ;;check
   (test-nonblocking-probe 0.0) ;;check
   (test-nonblocking-probe 1.1) ;;check
   (test-non-blocking-receive) ;;check
   #-cmu (test-non-blocking-send) ;;XXX TODO - broken on cmucl+mpich1.2
   (test-send-receive) ;;check
   (test-broadcast) ;;check?? enough??


   (test-send-auto) ;;check
   (test-broadcast-auto) ;;check

   (test-mpi-reduce) ;;check can't reduce the real-num data set.
   (test-mpi-reduce :allreduce t) ;;check

   (test-mpi-scatter) ;;?? can't scatter the array objects and the char objects
   (test-mpi-gather) ;;?? can't gather the array objects and the char objects
   (test-mpi-gather :all t) ;;?? test mpi-allgather
   (mpi-barrier)

   ;; test par-eval extension
   (test-blocking-spawn) ;;check
   (test-blocking-spawn2) ;;check
   (test-par-eval1) ;;check

   (formatp0 t "*test-failures*=~%~a~%" *test-failures*)
   (mpi-finalize)

   ))

#+nil(eval-when (:load-toplevel :execute)
      (testmpi)
      (mpi-finalize)
      #+sbcl(sb-ext:quit)
      #+cmu(ext:quit)
      )


