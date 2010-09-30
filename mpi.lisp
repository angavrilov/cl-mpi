#|

CL-MPI

MPI bindings for Common Lisp

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



Some of the documentation strings are copied or derived from:
 MPI: A Message-Passing Interface Standard
 Message Passing Interface Forum
 Version 1.1: June, 1995.
 which has the following copyright notice:
      (c) 1993, 1994, 1995 University of Tennessee, Knoxville,
      Tennessee. Permission to copy without fee all or part of this material
      is granted, provided the University of Tennessee copyright notice and
      the title of this document appear, and notice is given that copying is
      by permission of the University of Tennessee.
|#

(in-package #:mpi)

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf debug:*debug-print-level* 11          ;; default 3
        debug:*debug-print-length* 25)        ;; default 5
  (setf ext:*gc-verbose* nil) ;;shut up the garbage collector
  )

(defmacro formatp (stream format-string &rest rest)
  "For debugging CL-MPI.
   Like format, but attaches 'Proc #' and wtime to the output so that it's easier to understand
   where the messages are being generated"
  ;;(format t "format-string=~a, rest=~a~%" format-string rest)
  (let ((g-value (gensym)))
    `(progn (let ((,g-value (format ,stream ,(concatenate 'string "[~a :~,4f]: "  format-string) (mpi-comm-rank) (mpi-wtime) ,@rest)))
	      (force-output ,stream)
	      ,g-value))))

(defparameter *enable-mpi* t)
(defparameter *trace1* nil "toggle basic tracing")

(defmacro tracep (tracevar stream format-string &rest rest)
  "executes formatp when tracevar is non-nil"
  `(when ,tracevar
    (formatp ,stream ,(concatenate 'string "[~a :~,4f]: "  format-string) (mpi-comm-rank) (mpi-wtime) ,@rest)
    (force-output t)))

(defmacro formatp0 (stream format-string &rest rest)
  "formatp which only outputs for the rank0 node"
  (let ((g-value (gensym)))
    `(when (= 0 (mpi-comm-rank))
       (formatp ,stream ,format-string ,@rest))))

(defmacro call-mpi (mpi-api-function-call)
  (let ((mpi-fun-name (first mpi-api-function-call))
	(err (gensym)))
    `(when *enable-mpi*
       (let ((,err ,mpi-api-function-call))
	 ;;(format t "Called MPI function: ~a, err value=~a" (quote ,mpi-fun-name) ,err)
	 (when (/= 0 ,err)
           (error 'mpi-error :failed-function ',mpi-fun-name :error-code ,err))))))

(defun mpi-get-count (status mpi-type)
  (cffi:with-foreign-object (count :int)
    (call-mpi (MPI_Get_count status mpi-type count))
    (cffi:mem-aref count :int)))

(defun MPI_Status->mpi-status (status mpi-type); status is a CFFI pointe to an MPI_Status object
  "returns a Lisp-space status object, equivalent to the MPI_Status object"
  (make-status :count (if mpi-type
			  (mpi-get-count status mpi-type)
			  nil)
	       :source (cffi:foreign-slot-value status 'MPI_Status 'MPI_SOURCE)
	       :tag (cffi:foreign-slot-value status 'MPI_Status 'MPI_TAG)
	       :error (cffi:foreign-slot-value status 'MPI_Status 'MPI_ERROR)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment-related API
;;;

(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It is the only routine that may be called before MPI-INIT is called.
   [See MPI_INITIALIZED docs at http://www.mpi-forum.org/docs/mpi-11-html/node151.html]"
  (cffi:with-foreign-object (flag :int 1)
    (call-mpi (MPI_Initialized flag))
    (= 1 (cffi:mem-aref flag :int 0))))

(defun mpi-comm-rank (&optional(comm :MPI_COMM_WORLD))
  "indicates the number of processes involved in a communicator. For MPI_COMM_WORLD, it indicates the total number of processes available
  [See MPI_COMM_RANK docs at  http://www.mpi-forum.org/docs/mpi-11-html/node101.html]"
  (if *enable-mpi*
      (cffi:with-foreign-object (my-id :int)
	(call-mpi (MPI_comm_rank comm my-id))
	(cffi:mem-aref my-id :int))
      0))

(defun mpi-comm-size ()
  "indicates the number of processes involved in a communicator. For MPI_COMM_WORLD, it indicates the total number of processes available
  [See MPI_COMM_SIZE docs at  http://www.mpi-forum.org/docs/mpi-11-html/node101.html]"
  (cffi:with-foreign-object (numprocs :int)
    (call-mpi (MPI_comm_size :MPI_COMM_WORLD numprocs))
    (cffi:mem-aref numprocs :int)))

(defun mpi-finalize()
  "This routines cleans up all MPI state. Once this routine is called, no MPI routine (even MPI-INIT) may be called. The user must ensure that all pending communications involving a process completes before the process calls MPI-FINALIZE.
   [See MPI_FINALIZE docs at http://www.mpi-forum.org/docs/mpi-11-html/node151.html]"
  (tracep *trace1* t "Calling MPI_Finalize~%")
  (call-mpi (MPI_Finalize)))

(defun mpi-abort(&key (comm :MPI_COMM_WORLD)(errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of comm. This function does not require that the invoking environment take any action with the error code. However, a Unix or POSIX environment should handle this as a return errorcode from the main program or an abort(errorcode).
   [See MPI_ABORT docs at http://www.mpi-forum.org/docs/mpi-11-html/node151.html]"
  (cffi:with-foreign-object (c-errcode :int)
    (setf (cffi:mem-aref c-errcode :int) errcode)
    (call-mpi (MPI_Abort comm (cffi:mem-aref c-errcode :int)))
    c-errcode))

(defun mpi-init ()
  "This routine must be called before any other MPI routine. It must be called at most once; subsequent calls are erroneous (see MPI-INITIALIZED).

All MPI programs must contain a call to MPI-INIT; this routine must be called before any other MPI routine (apart from MPI-INITIALIZED) is called.
   [See MPI_INIT docs at  http://www.mpi-forum.org/docs/mpi-11-html/node151.html]"

  (let* ((command-line-args #+sbcl sb-ext:*posix-argv*
			    #+cmu ext:*command-line-strings*)
	 (num-args (length command-line-args)))
    (cffi:with-foreign-object (argc :int 1)
      (setf (cffi:mem-aref argc :int 0) num-args)
      (let ((argv (cffi:foreign-alloc :string
				 :initial-contents command-line-args
				 :null-terminated-p t)))
	(cffi:with-foreign-object (argvp :pointer 1)
	  (setf (cffi:mem-aref argvp :pointer 0) argv)
	  (call-mpi (MPI_Init argc argvp))
	  ;;(call-mpi (MPI_Init (cffi:null-pointer) (cffi:null-pointer)))
	  )
	(cffi:foreign-free argv)))))

(defmacro with-mpi (&body body)
  "executes body in an MPI environment (initializes and finalizes MPI before/after body)"
  `(progn
     (mpi-init)
     (unwind-protect (progn ,@body)
       (mpi-finalize))))

(defun mpi-get-processor-name ()
  "
   This routine returns the name of the processor on which it was called at the moment of the call.
   The name is a character string for maximum flexibility. From this value it must be possible to identify a specific piece of hardware;
   possible values include 'processor 9 in rack 4 of mpp.cs.org' and '231' (where 231 is the actual processor number in the running homogeneous system).

   [See MPI_GET_PROCESSOR_NAME docs at  http://www.mpi-forum.org/docs/mpi-11-html/node143.html]"
  (cffi:with-foreign-object (namelen :int) ;length of name returned by MPI call
    (cffi:with-foreign-pointer (processor-name MPI_MAX_PROCESSOR_NAME)
      (MPI_Get_processor_name processor-name namelen)
      (tracep *trace1* t "namelen=~a, processor-name=~a, namelen as lisp=~a~%" namelen processor-name (cffi:mem-aref namelen :int))
      ;(cffi:foreign-string-to-lisp processor-name (cffi:mem-aref namelen :int) nil)
      (cffi:foreign-string-to-lisp processor-name :count (cffi:mem-aref namelen :int))
      )))

;;Syntax
;;— Function: foreign-string-to-lisp ptr &optional offset count max-chars encoding ⇒ string

(defun mpi-wtime ()
  "Returns a (double) floating-point number of seconds, representing elapsed wall-clock time since some time in the past.

  The 'time in the past' is guaranteed not to change during the life of the process.
   The user is responsible for converting large numbers of seconds to other units if they are preferred.

  This function is portable (it returns seconds, not 'ticks'), it allows high-resolution, and carries no unnecessary baggage.

  The times returned are local to the node that called them. There is no requirement that different nodes return 'the same time.'

  [See MPI_WITME docs at  http://www.mpi-forum.org/docs/mpi-11-html/node150.html]"
  ;;TODO (But see also the discussion of MPI_WTIME_IS_GLOBAL).
  (MPI_Wtime));does not return error, so don't wrap!

(defun mpi-wtick ()
  "Returns the resolution of MPI-WTIME in seconds. That is, it returns, as a double precision value,
    the number of seconds between successive clock ticks. For example,
    if the clock is implemented by the hardware as a counter that is incremented every millisecond,
    the value returned by MPI-WTICK should be 0.001

    [See MPI_WTICK docs at http://www.mpi-forum.org/docs/mpi-11-html/node150.html]"
  (MPI_Wtick));does not return error, so don't wrap!


(defun mpi-barrier (&optional (comm :MPI_COMM_WORLD))
  "MPI_BARRIER blocks the caller until all group members have called it. The call returns at any process only after all group members have entered the call.
   [See MPI_BARRIER docs at http://www.mpi-forum.org/docs/mpi-11-html/node66.html]"
;  (MPI_Barrier (cffi:foreign-enum-value 'MPI_Comm :MPI_COMM_WORLD))
  (call-mpi (MPI_Barrier comm)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Metadata manipulation
;;;
;;; ** This is very inefficient -- need to change implementation to something like a hash table
;;;

(defparameter *lisp-cffi-mpi-conversions*
  '(
    (bit 1 :MPI_CHAR :char 0)
    ((signed-byte 8) 1 :MPI_CHAR :char 0)
    ((unsigned-byte 8) 1 :MPI_UNSIGNED_CHAR :unsigned-char 1)
    ((signed-byte 16) 2 :MPI_SHORT :short 2)
    ((unsigned-byte 16) 2 :MPI_UNSIGNED_SHORT :unsigned-short 2)
    ((signed-byte 32) 4 :MPI_INT :int 3)
    ((unsigned-byte 32) 4 :MPI_UNSIGNED :unsigned-int 4)
    (single-float 4 :MPI_FLOAT :float 5)
    (double-float 8 :MPI_DOUBLE :double 6)
    (character  1 :MPI_UNSIGNED_CHAR :unsigned-char 7)
    (fixnum 4 :MPI_INT :int 8) ; XXX TODO What about 64-bit fixnums?
    )
  "basic mappings between lisp types, cffi tyes, and mpi types")
(defun typespec-lisp-type (s) (first s))
(defun typespec-size (s) (second s))
(defun typespec-mpi-type (s) (third s))
(defun typespec-cffi-type (s) (fourth s))
(defun typespec-id (s) (fifth s))


(defconstant +converted-object+ 1)
(defconstant +simple-array+ 2)
(defconstant +string+ 3)
(defconstant +base-object+ 4)

(defconstant +metadata-tag+ 32767)


(defun get-typespec (lisp-type)
  (loop for type-spec in *lisp-cffi-mpi-conversions* do
	(when (equal lisp-type (first type-spec))
	  (return-from get-typespec (values (first type-spec) (second type-spec) (third type-spec) (fourth type-spec)(fifth type-spec)))))
  (values nil nil nil nil))

(defun match-typespec (object)
  "Match basic typespec for object (values lisp-type bytes mpi-type cffi-type type-id object)"
  (loop for type-spec in *lisp-cffi-mpi-conversions* do
	(when (typep object (first type-spec))
	  (return-from match-typespec (values (first type-spec) (second type-spec) (third type-spec) (fourth type-spec)(fifth type-spec) object))))
  (values nil nil nil nil nil object))



(defun is-simple-array (object)
  (let ((type (type-of object)))
    (and (subtypep type '(simple-array * *))
	 (> (length type) 2) ; rule out (simple-vector *)
	 )))


(defun is-string-type (type)
  (subtypep type '(simple-array character *)))


(defun to-string (d)
  "Converts d to a string which can be READ"
  (prin1-to-string d))


(defun obj-tspec->id (obj-tspec)
  "Determine whether the object referred to by this metadata is a converted object, a string, a simple-array, or a base-object"
  (cond ((obj-tspec-converted-obj obj-tspec)
	 +converted-object+)
	((subtypep (obj-tspec-type obj-tspec) '(simple-array character *))
	 +string+)
	((subtypep (obj-tspec-type obj-tspec) '(simple-array * *))
	 +simple-array+)
	(t
	 +base-object+)))


(defun find-typespec (lisp-type)
  (find lisp-type *lisp-cffi-mpi-conversions* :test (lambda (x y) ;;(format t "x=~a, y=~a~%" x y)
						      (subtypep x (first y)))))

(defun lisp-type->mpi-type (lisp-type)
  (typespec-mpi-type (find-typespec lisp-type)))
;  (multiple-value-bind (lisp-type size mpi-type cffi-type id)
;      (get-typespec lisp-type)
;    mpi-type))


(defun get-typespec-by-index (lisp-type-index)
  (loop for ts in *lisp-cffi-mpi-conversions* do
	(when (equal lisp-type-index (typespec-id ts))
	  (return-from get-typespec-by-index (values (typespec-lisp-type ts) (typespec-size ts) (typespec-mpi-type ts) (typespec-cffi-type ts)(typespec-id ts)))))
  (values nil nil nil nil nil))

(defun type->index (lisp-type)
  (typespec-id (find lisp-type *lisp-cffi-mpi-conversions* :test #'(lambda (x y) (equal x (typespec-lisp-type y))))))

(defun object->basetype (object)
  "Match basic typespec for object (values lisp-type bytes mpi-type cffi-type type-id object)"
  ;;(find (type-of object) *lisp-cffi-mpi-conversions* :test #'(lambda (x y) (equal x (typespec-lisp-type y)))))
  (find (type-of object) *lisp-cffi-mpi-conversions*
	:test #'(lambda (x y) (subtypep x (typespec-lisp-type y)))))


(defun match-type (object &key (enable-default-conversion t))
  "Returns (values lisp-type lisp-base-type count mpi-type cffi-type type-id object)
   If enable-default-conversion is t, object is possibly converted (e.g., to a string)"
  (declare (optimize (speed 0) (debug 3)))
  (let ((base-typespec (object->basetype object)))
    (cond (base-typespec
	   (make-obj-tspec :type (type-of object) :count 1 :base-typespec base-typespec))
	  ((stringp object) ;;??? XXX is type string or (simple-arrah character *)?
	   (make-obj-tspec :type '(simple-array character *) :count (length object) :base-typespec (find-typespec 'character)))
	  ((is-simple-array object)
	   (let* ((base-type (second (type-of object)))
		  (base-typespec (find-typespec base-type)))
	     (assert base-typespec)
	     (tracep *trace1* t "base-type=~a~%" base-type)
	     (make-obj-tspec :type 'simple-array :count (length object) :base-typespec base-typespec)))
	  ;; generic conversion to READable string for objects which are not basic and not simple-array
	  (t ;
	   (cond (enable-default-conversion ; convert any lisp object to a string
		  (let ((obj-string (to-string object)))
		    (make-obj-tspec :type '(simple-array character *) :count (length obj-string)
				    :base-typespec (find-typespec 'character)
				    :converted-obj obj-string)))
		 (t
		  (assert nil)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defconstant +default-tag+ 1 "default tag for MPI_Send and MPI_Recv")

(defparameter *message-buffer* nil "The buffer associated with this process")

(defun mpi-buffer-attach (buf-size-bytes)
  "Provides to MPI a buffer in the user's memory to be used for buffering outgoing messages. The buffer is used only by messages sent in buffered mode. Only one buffer can be attached to a process at a time.

   MPI-BUFFER-ATTACH attaches a NEWLY ALLOCATED system message buffer to this process.
   Assigns *message-buffer* to the CFFI pointer to this new buffer.
   Also returns the CFFI pointer to this buffer

   [See MPI_BUFFER_ATTACH and MPI_BUFFER_DETACH docs at http://www.mpi-forum.org/docs/mpi-11-html/node42.html]
"
  (assert (null *message-buffer*)) ;only one buffer can be attached to a process
  (let ((p (cffi:foreign-alloc :char :count buf-size-bytes)))
    (call-mpi (MPI_Buffer_attach p buf-size-bytes))
    (setf *message-buffer* p)
    p))

(defparameter *pointer-size* 8)

(defun mpi-buffer-detach ()
  "Detach the buffer currently associated with MPI. The call returns
   the size of the detached buffer. This operation will
   block until all messages currently in the buffer have been
   transmitted. Upon return of this function, the user may reuse or
   deallocate the space taken by the buffer.
   *** Returns the size of the deallocated buffer ONLY.
       Unlike the standard function, this Lisp binding will NOT return the address of
       the deallocated buffer. This is because I don't believe I should try to support the
       the 'nested libraries' scenario which is the rationale for returning the address
       of the deallocated buffer
         (see http://www-unix.mcs.anl.gov/mpi/www/www3/MPI_Buffer_detach.html)
   *** Unlike the standard, I DEALLOCATE the CFFI-allocated space associated with the buffer
   [See MPI_BUFFER_ATTACH and MPI_BUFFER_DETACH docs at http://www.mpi-forum.org/docs/mpi-11-html/node42.html]"
  (assert *message-buffer*)
  (cffi:with-foreign-pointer (deallocated-buf-address *pointer-size*)
    (cffi:with-foreign-object (c-deallocated-bytes :int)
      ;;(formatp t "*message-buffer*=~a~%" *message-buffer*)
      (MPI_Buffer_detach deallocated-buf-address c-deallocated-bytes)
;      (cffi:foreign-free *message-buffer*)
      #+nil(formatp t "deallocated-buf-address=~a, deallocated ~a bytes~%"
	      deallocated-buf-address (cffi:mem-aref c-deallocated-bytes :int))
      (setf *message-buffer* nil)
      (cffi:mem-aref c-deallocated-bytes :int))))

(defmacro with-buffer (buf-size &body body)
  "Creates a new system message buffer with size buf-size, performs the body, then detaches and deallocates the buffer
   [See MPI_BUFFER_ATTACH and MPI_BUFFER_DETACH docs at http://www.mpi-forum.org/docs/mpi-11-html/node42.html]"
  `(progn
    (mpi-buffer-attach ,buf-size)
    ,@body
    (mpi-buffer-detach)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic point-to-point operations
;;;

(defun mpi-send-1 (buf count mpi-type destination &key (tag +default-tag+)
		   (mode :basic)(comm :MPI_COMM_WORLD))
  "A low-level wrapper around the MPI_Send, MPI_Ssend, MPI_Rsend, and MPI_Bsend functions"
  (case mode
    (:basic  (call-mpi (MPI_Send buf count mpi-type destination  tag comm)))
    (:synchronous (call-mpi (MPI_Ssend buf count mpi-type destination  tag comm)))
    (:ready (call-mpi (MPI_Rsend buf count mpi-type destination tag comm)))
    (:buffered (call-mpi (MPI_Bsend buf count mpi-type destination  tag comm)))))


(defun mpi-send(data destination &key (tag +default-tag+) (mode :basic)(comm :MPI_COMM_WORLD))
  "Blocking send fuction. Sends data to definition.
   Tries to be smart about automatically packaging the data depending on its type, and
   Handle standard MPI types and CL objects
  [See related docs on blocking send (MPI_SEND) http://www.mpi-forum.org/docs/mpi-11-html/node31.html,
   communication modes at http://www.mpi-forum.org/docs/mpi-11-html/node40.html]
  "
  (let* ((metadata (match-type data))
	 (base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec->id metadata))
	 (cffi-type (typespec-cffi-type base-typespec))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)))
    (assert (and metadata base-typespec meta-id))
    (tracep *trace1* t "Send1: lisp-type=~a, count=~a, base-type=~a ~%" (obj-tspec-type metadata)  count base-typespec)
    ;; This function will only handle strings, simple arrays, and base objects!
    (assert (or (= meta-id +string+) (= meta-id +simple-array+)(= meta-id +base-object+)))
    (when (= +string+ meta-id)
      (cffi:with-foreign-string (cs data)
	(mpi-send-1 cs count :MPI_CHAR  destination :tag tag :comm comm :mode mode)))

    (cffi:with-foreign-object (buf (typespec-cffi-type base-typespec) (obj-tspec-count metadata))
      (cond ((= +simple-array+ meta-id)
	     (loop for i from 0 below count do
		   (setf (cffi:mem-aref buf cffi-type i)(aref data i)))
	     (mpi-send-1 buf count mpi-type destination :tag tag :comm comm :mode mode))
	    ((= +base-object+ meta-id)
	     (setf (cffi:mem-aref buf cffi-type) data)
	     (mpi-send-1 buf 1 mpi-type destination :tag tag :comm comm :mode mode))))
      ))

(defun mpi-receive (source type count &key (tag +default-tag+)(comm :MPI_COMM_WORLD))
  "Smart receiver protocol -- assumes that receiver knows count of the data has an appropriate count buffer ready to receive
   intended to matches call to mpi-send
   Blocking receive function.
   Returns (values data status)
  [See MPI_RECV docs at  http://www.mpi-forum.org/docs/mpi-11-html/node34.html]
  will allocate and return a new object.
  "
  (when (is-string-type type)
    (return-from mpi-receive (mpi-receive-string source :tag tag :buf-size-bytes (* 2 count))))

  (let* ((base-type (if (subtypep type '(simple-array * *)) (second type) type))
	 (base-typespec (find-typespec base-type))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (cffi-type (typespec-cffi-type base-typespec)))
    (assert base-typespec)
    (cffi:with-foreign-objects ((mpi-status 'MPI_Status))
      (cffi:with-foreign-object  (buf cffi-type count)
	(call-mpi (MPI_Recv buf count mpi-type source tag comm (cffi:mem-aref mpi-status 'MPI_Status)))
	(let ((status (MPI_Status->mpi-status mpi-status mpi-type)))
	  (tracep *trace1* t "received object ~a, status=~a~%" (cffi:mem-aref buf cffi-type) status)
	  (values (if (subtypep type '(simple-array * *))
		      (let ((a (make-array count :element-type base-type)))
			(loop for i from 0 below count do (setf (aref a i) (cffi:mem-aref buf cffi-type i)))
			a)
		      (cffi:mem-aref buf cffi-type))
		  status))))))

(defun mpi-receive1 (source type count &key (tag +default-tag+)(comm :MPI_COMM_WORLD))
  "Smart receiver protocol -- assumes that receiver knows type of data to receive
   intended to matches call to mpi-send
   Blocking receive function.
   Returns (values data status)
  [See MPI_RECV docs at  http://www.mpi-forum.org/docs/mpi-11-html/node34.html]
  will allocate and return a new object.
  "
  (let* ((base-type (cond ((is-string-type type)
			   'character
			  )
			 ((subtypep type '(simple-array * *))
			  (second type) )
			 (t
			  type)))
	 (status (mpi-probe source tag :base-type base-type))
	 (count (status-count status)))
    (assert count)
    (tracep *trace1* t "mpi-receive1 probed: status=~a~%" status)
    (mpi-receive source type count :tag tag :comm comm)))


(defun mpi-send-string (s destination &key (tag +default-tag+) (mode :basic)(blocking :t)(comm :MPI_COMM_WORLD))
  "send string to destination
   mode is one of:  :basic  :buffered :synchronous
   blocking can be t or nil.
   When blocking is t,  this call returns only after the
   application buffer in the sending task is free for reuse. Note that
   this routine may be implemented differently on different systems. The
   MPI standard permits the use of a system buffer but does not require
   it. Some implementations may actually use a synchronous send (MPI_Ssend)
   to implement the basic blocking send.
   If blocking is nil, then a non-blocking send operation is used.


   if synchronous == t, then uses  MPI_Ssend, otherwise uses MPI_Send
   Send vs Bsend vs Ssend:(let ((req (mpi-receive-string-noblock
   In case of Send, nothing is guaranteed about the state of the receiver.
      If ther receiver is not ready to read the data, then the data may be
      stored in a system buffer.
      However, it *might* be unsafe to assume anything about the size of the
      the system buffer, which might overflow.
    Ssend guarantees that we block until the recipient starts reading the data.
    Bsend explicitly states that a buffer will be used. Furthermore, the buffer that is used is allocated by the user using attach_buffer. Thus, using Bsend makes it explicit that we are relying on a buffer, and furthermore, we have control over the size of the buffer. In basic Send, a buffer might be used (or not), and we don't have any guarantees about the size of the buffer.

  [See related docs on blocking send (MPI_SEND) http://www.mpi-forum.org/docs/mpi-11-html/node31.html,
   communication modes at http://www.mpi-forum.org/docs/mpi-11-html/node40.html]
   "
  (let ((count (length s)))
    (assert (< count *mpi-string-buf-size*))
    (cond (blocking
	   (cffi:with-foreign-string (c-str s)
	     (mpi-send-1 c-str count :MPI_CHAR  destination :tag tag :comm comm :mode mode)))
	  (t ;non-blocking
	   ;; must return a request handle
	   (let ((request (cffi:foreign-alloc 'MPI_Request))
		 (buf (cffi:foreign-alloc :char :count count)))
             (cffi:lisp-string-to-foreign s buf (1+ count));; need to add 1 to count (and also null-terminates the string)
	     (case mode
	       (:basic (call-mpi (MPI_Isend buf count :MPI_CHAR destination tag comm request)))
	       (:synchronous (call-mpi (MPI_Issend buf count :MPI_CHAR destination tag comm request)))
	       (:buffered (call-mpi (MPI_Ibsend buf count :MPI_CHAR destination tag comm request))))
	     (tracep *trace1* t "mpi-send-string generated request = ~a~%" request)
	     (return-from mpi-send-string (make-request :mpi-request request :buf buf)))))))

(defun mpi-receive-string (source &key (tag +default-tag+) (buf-size-bytes *mpi-string-buf-size*))
  "Blocking receive string.
   Returns (values string count)
   INEFFICIENT - shouldn't allocate buffer every time."
  (declare (type (unsigned-byte 32) source tag))
  (cffi:with-foreign-objects ((status 'MPI_Status))
    (cffi:with-foreign-pointer (buf buf-size-bytes)
;	(bogo-mpi-recv buf (cffi:mem-aref c-buf-size-bytes :int) :MPI_CHAR (cffi:mem-aref c-source :int)
;      (call-mpi (MPI_Recv buf buf-size-bytes :MPI_CHAR source tag :MPI_COMM_WORLD (cffi:mem-aref status 'MPI_Status)))
      (call-mpi (MPI_Recv buf buf-size-bytes :MPI_BYTE source tag :MPI_COMM_WORLD (cffi:mem-aref status 'MPI_Status)))

      (let ((count (cffi:foreign-slot-value status 'MPI_Status 'count)))
	;;(formatp t "received string len ~a = ~a~%" count (cffi:foreign-string-to-lisp buf count nil))
	(values (cffi:foreign-string-to-lisp buf :count count) count)))))

(defun mpi-ireceive-string (source &key (tag +default-tag+) (buf-size-bytes *mpi-string-buf-size*))
  "Blocking receive string.
   Returns (values string count)
   INEFFICIENT - shouldn't allocate buffer every time."
  (declare (type (unsigned-byte 32) source tag))
  (cffi:with-foreign-objects ((status 'MPI_Status))
    (cffi:with-foreign-pointer (buf buf-size-bytes)
      ;;(formatp t "receiving string"); len ~a = ~a~%" count (cffi:foreign-string-to-lisp buf count nil))
;	(bogo-mpi-recv buf (cffi:mem-aref c-buf-size-bytes :int) :MPI_CHAR (cffi:mem-aref c-source :int)
      (call-mpi (MPI_Irecv buf buf-size-bytes :MPI_CHAR source tag :MPI_COMM_WORLD (cffi:mem-aref status 'MPI_Status)))
      (let ((count (cffi:foreign-slot-value status 'MPI_Status 'count)))
	;(formatp t "received string len ~a = ~a~%" count (cffi:foreign-string-to-lisp buf count nil))
	(values (cffi:foreign-string-to-lisp buf :count count) count)))))



(defun mpi-send-receive-string (send-str destination source
				&key (send-tag +default-tag+)(recv-tag +default-tag+)
				(comm :MPI_COMM_WORLD)(recv-buf-size-bytes *mpi-string-buf-size*))
  "blocking send-receive operation.
   Returns (values received-string size-of-received-message)"
  (cffi:with-foreign-string (c-send-str send-str)
    (cffi:with-foreign-objects ((status 'MPI_Status)) ; the received status object
      (cffi:with-foreign-pointer (recv-buf recv-buf-size-bytes)
	(call-mpi (MPI_Sendrecv
		   c-send-str (length send-str) :MPI_CHAR destination send-tag
		   recv-buf recv-buf-size-bytes :MPI_CHAR source  recv-tag
		   comm (cffi:mem-aref status 'MPI_Status)))
	(let ((count (cffi:foreign-slot-value status 'MPI_Status 'count)))
	  ;;(values (cffi:foreign-string-to-lisp recv-buf count nil) :count count))))))
	  (values (cffi:foreign-string-to-lisp recv-buf :count count) count))))))

(defun mpi-send-auto (data destination &key (tag +default-tag+)
		      (mode :basic)(comm :MPI_COMM_WORLD))
  "Sends data to destination. Data is any Lisp object.
   INEFFICIENT: First sends metadata (type, count) which is necessary to set up the second
   send, which is the actual data payload.
   "
  (declare (optimize (debug 3)(speed 0)(safety 3)))
  (let ((metadata (match-type data)))
    ;; first, send the type and size as a 2-element array of ints
    (tracep *trace1* t "send-auto metadata = ~a~%" metadata)
    (cffi:with-foreign-object (array :int 3)
      (setf (cffi:mem-aref array :int 0) (typespec-id (obj-tspec-base-typespec metadata)))
      (setf (cffi:mem-aref array :int 1) (obj-tspec-count metadata))
      (setf (cffi:mem-aref array :int 2) (obj-tspec->id metadata))
      (tracep *trace1* t "send-auto, metadata packed id=~a, count=~a, id=~a~%" (cffi:mem-aref array :int 0)(cffi:mem-aref array :int 1)(cffi:mem-aref array :int 2))
      (mpi-send-1 array  3 :MPI_INT destination :tag +metadata-tag+ :mode mode :comm comm))

    ;; send the actual payload
    (cond ((obj-tspec-converted-obj metadata)
	   (let ((converted-data (obj-tspec-converted-obj metadata)))
	     (assert (stringp converted-data))
	     (cffi:with-foreign-string (c-str converted-data)
	       (mpi-send-1 c-str (length converted-data)  :MPI_CHAR  destination :tag tag :comm comm :mode mode))))
	  ((stringp data)
	   (cffi:with-foreign-string (c-str data)
	     (mpi-send-1 c-str (length data)  :MPI_CHAR  destination :tag tag :comm comm :mode mode)))
	  (t ; a non-converted, basic object (a base object or a simple-array of base objects)
	   (let ((base-typespec (obj-tspec-base-typespec metadata)))
	     (cffi:with-foreign-object (buf (typespec-cffi-type base-typespec) (obj-tspec-count metadata))
	       (cond ((is-simple-array data)
		      (loop for i from 0 below (obj-tspec-count metadata) do
			    (setf (cffi:mem-aref buf (typespec-cffi-type base-typespec) i)(aref data i))))
		     (t ;a basic object
		      (setf (cffi:mem-aref buf (typespec-cffi-type base-typespec)) data)))
	       (mpi-send-1 buf (obj-tspec-count metadata) (typespec-mpi-type base-typespec) destination
			 :tag tag :comm comm :mode mode)))))
    ))

(defun mpi-receive-auto (source &key (tag +default-tag+)(comm :MPI_COMM_WORLD))
  "Receives data from the source.
   INEFFICIENT: First receives metadata (type, count) which is necessary to set up the second
   receive call, which is the actual data payload."
  (declare (optimize (debug 3)(speed 0)(safety 3)))
  (let ((count 0)
	(base-type-id nil)
	(meta-id nil))
    ;; First, receive metadata
    (cffi:with-foreign-objects ((mpi-status 'MPI_Status))
      (cffi:with-foreign-object (array :int 3)
	(call-mpi (MPI_Recv array 8 :MPI_INT source +metadata-tag+ comm (cffi:mem-aref mpi-status 'MPI_Status)))
	(let ((status (MPI_Status->mpi-status mpi-status :MPI_INT)))
	  (declare (ignore status));;XXX TEMP
	  (setf base-type-id (cffi:mem-aref array :int 0))
	  (setf count (cffi:mem-aref array :int 1))
	  (setf meta-id (cffi:mem-aref array :int 2))
	  )))
    ;; units of 'count' is # of base-type objects
    ;; Receive the payload
    (tracep *trace1* t "Receive: base-type-id=~a, count=~a, meta-id=~a~%" base-type-id count meta-id)
    (cond ((= +converted-object+ meta-id) ; object sent as READ'able string
	   (read-from-string (mpi-receive-string source :tag tag :buf-size-bytes count)))
	  ((= +string+ meta-id)
	   (mpi-receive-string source :tag tag :buf-size-bytes count))
	  (t ;either a basic object or a simple-array of basic objects
	   (multiple-value-bind (base-lisp-type base-lisp-type-size mpi-type cffi-type)
	       (get-typespec-by-index base-type-id)
	     (declare (ignore base-lisp-type-size))
	     (tracep *trace1* t "Receive: base-lisp-type=~a, count=~a, mpi-type=~a, cffi-type=~a~%" base-lisp-type count mpi-type cffi-type)
	     (assert base-lisp-type)
	     (cffi:with-foreign-objects ((mpi-status 'MPI_Status))
	       (cffi:with-foreign-object (buf cffi-type count)
		 (call-mpi (MPI_Recv buf count mpi-type source tag comm (cffi:mem-aref mpi-status 'MPI_Status)))
		 (let ((status (MPI_Status->mpi-status mpi-status mpi-type)))
		   (declare (ignore status))
		   ;;(tracep *trace1* t "received object ~a, status=~a~%" (cffi:mem-aref buf cffi-type) status)
		   (cond ((= +simple-array+ meta-id);(> count 1)
			  (make-array count :element-type base-lisp-type :initial-contents
				      (loop for i from 0 below count collect (cffi:mem-aref buf cffi-type i))))
			 (t ;(= +base-object+ meta-id)
			  (cffi:mem-aref buf cffi-type)))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; non-blocking communications
;;;

(defun request-get-string (req count)
  "Returns (values msg count) for the string that is stored in a request's buffer"
  (let* ((buf (request-buf req))
	 (msg (cffi:foreign-string-to-lisp buf :count count)))
    ;;(cffi:foreign-free buf)
    (values  msg count)))


(defun mpi-wait (request)
  "block until request completes"
  (cffi:with-foreign-object (mpi-status 'MPI_Status)
    (tracep *trace1* t "request=~a" request)
    (call-mpi (MPI_Wait (request-mpi-request request)  mpi-status))
    (tracep *trace1* t "count=~a,source=~a,tag=~a,err=~a~%" (mpi-get-count mpi-status :MPI_CHAR)
	     (cffi:foreign-slot-value mpi-status 'MPI_Status 'MPI_SOURCE)
	     (cffi:foreign-slot-value mpi-status 'MPI_Status 'MPI_TAG)
	     (cffi:foreign-slot-value mpi-status 'MPI_Status 'MPI_ERROR))
    (MPI_Status->mpi-status mpi-status nil)))


(defun copy-requests-sequence-to-c-array (num-requests requests c-requests)
  (loop for r from 0 below num-requests
     do (setf (cffi:mem-aref c-requests 'MPI_Request r)
              (cffi:mem-ref (request-mpi-request (elt requests r)) 'MPI_Request))))

(defun mpi-wait-any (requests)
  "block until one request completes.
   Returns the status for the completed request"
  (let ((num-requests (length requests)))
    (cffi:with-foreign-objects ((mpi-status 'MPI_Status)
				;(completed-index :int)
				(completed-index :pointer)
				(c-requests 'MPI_Request num-requests))
      (copy-requests-sequence-to-c-array num-requests requests c-requests)
      (call-mpi (MPI_Waitany num-requests c-requests completed-index mpi-status))
      (values (cffi:mem-aref completed-index :int) (MPI_Status->mpi-status mpi-status nil)))))

(defun mpi-wait-any2 (requests)
  "returns (values received_request status requests_minus_completed_request)
   (this version is so that we don't have to keep track of request indices"
  (multiple-value-bind (completed-index status)
      (mpi-wait-any requests)
    (values (elt requests completed-index) status (remove (elt requests completed-index) requests))))



(defun mpi-wait-all (requests)
  "block until all request completes.
   Returns array of statuses, where status[i] = status of request [i]"
  ;; XXX BROKEN ?? can't seem to allocate foreign object with a variable argument?
  (let* ((num-requests (length requests)))
    (tracep *trace1* t "num-requests = ~a, requests=~a" num-requests requests)
    (cffi:with-foreign-objects ((mpi-statuses 'MPI_Status num-requests)
				(c-requests 'MPI_Request num-requests))
      (copy-requests-sequence-to-c-array num-requests requests c-requests)
      (call-mpi (MPI_Waitall num-requests c-requests mpi-statuses))
      ;;(formatp t "past MPI_Waitall call")
      (let ((statuses (make-array num-requests)))
	(loop for i from 0 below num-requests do
	      (setf (aref statuses i) (MPI_Status->mpi-status (cffi:mem-aref mpi-statuses 'MPI_Status i) nil)))
	statuses))))

#+nil
(defun mpi-wait-all (requests)
  "block until all request completes.
   Returns array of statuses, where status[i] = status of request [i]"
  ;; XXX BROKEN ?? can't seem to allocate foreign object with a variable argument?
  (assert nil)
  (let* ((num-requests (length requests))
	 (num-requests2 (* num-requests 1))
	 (bogo 1))
    (tracep *trace1* t "num-requests=~a, requests=~a" num-requests requests)
    (cffi:with-foreign-objects (;(mpi-status 'MPI_Status)
				;(mpi-statuses 'MPI_Status num-requests)
				;(mpi-statuses 'MPI_Status 1)
				(c-requests :pointer bogo)
				;(c-requests :pointer 1) ;; but it looks like allocating with a static param is OK ???
				)
      (break)
      )))
;;       (copy-requests-sequence-to-c-array num-requests requests c-requests)
;;       (call-mpi (MPI_Waitall num-requests c-requests mpi-statuses))
;;       ;;(formatp t "past MPI_Waitall call")
;;       (let ((statuses (make-array num-requests)))
;; 	(loop for i from 0 below num-requests do
;; 	      (setf (aref statuses i) (MPI_Status->mpi-status (cffi:mem-aref mpi-statuses 'MPI_Status i) nil)))
;; 	statuses))))

(defun mpi-test-all (requests)
  "checks status of all requests
   Returns array of statuses, where status[i] = status of request [i]"
  (let ((num-requests (length requests)))
    (cffi:with-foreign-objects ((mpi-statuses 'MPI_Status num-requests)
				(flag :int)
				(c-requests 'MPI_Request num-requests))
      (copy-requests-sequence-to-c-array num-requests requests c-requests)
      (call-mpi (MPI_Testall num-requests c-requests flag mpi-statuses))
      ;;(formatp t "past MPI_Testall call")
      (let ((statuses (make-array num-requests)))
	(loop for i from 0 below num-requests do
	      (setf (aref statuses i) (MPI_Status->mpi-status (cffi:mem-aref mpi-statuses 'MPI_Status i) nil)))
	(values (= 1 (cffi:mem-aref flag :int)) statuses)))))

(defun mpi-test-any (requests)
  (let ((num-requests (length requests)))
    (cffi:with-foreign-objects ((mpi-status 'MPI_Status)
				(out-index :int)
				(flag :int)
				(c-requests 'MPI_Request num-requests))
      (copy-requests-sequence-to-c-array num-requests requests c-requests)
      (call-mpi (MPI_Testany num-requests c-requests out-index flag mpi-status))
      ;;(formatp t "past MPI_Testany call")
      (values (= 1 (cffi:mem-aref flag :int)) (cffi:mem-aref out-index :int) (MPI_Status->mpi-status mpi-status nil)))))

(defun mpi-test (request)
  "test whether request completed"
  (cffi:with-foreign-object (flag :int)
    (cffi:with-foreign-object (status 'MPI_Status)
      (call-mpi (MPI_Test (request-mpi-request request) flag status))
      (values (= 1 (cffi:mem-aref flag :int)) (MPI_Status->mpi-status status nil)))))

(defun mpi-wait/test-some (requests mode)
  "block until some request(s) complete.
   Returns list of cons cells, car is index of completed request, cdr is its status"
  (let ((num-requests (length requests)))
    (cffi:with-foreign-objects ((mpi-statuses 'MPI_Status num-requests)
				(c-done-indices :int num-requests)
				(c-outcount :int)
				(c-requests 'MPI_Request num-requests))
      (copy-requests-sequence-to-c-array num-requests requests c-requests)
      (case mode
	(:wait
	 (call-mpi (MPI_Waitsome num-requests c-requests c-outcount c-done-indices mpi-statuses)))
	(:test
	 (call-mpi (MPI_Waitsome num-requests c-requests c-outcount c-done-indices mpi-statuses))))
      (let ((num-done (cffi:mem-aref c-outcount :int)))
	(tracep *trace1* t "num-done=~a~%" num-done)
	(loop for i from 0 below num-done collect
	      (cons (cffi:mem-aref c-done-indices :int i) ;nil))))))
		    (MPI_Status->mpi-status (cffi:mem-aref mpi-statuses 'MPI_Status  i) nil)))))))

(defun mpi-wait/test-some2 (requests mode)
  "returns (values (list_of (cons received_request status)) remaining-requests
   INEFFICIENT - but this is a complex operation so don't optimize unless necessary"
  (let* ((num-original-requests (length requests))
	 (done-index-status-pairs (mpi-wait/test-some requests mode))
	 (done-indices (loop for pair in done-index-status-pairs collect (car pair)))
	 (remaining-indices (set-difference (loop for i from 0 below num-original-requests collect i)
					    done-indices)))
    (values (loop for index-status-pair in done-index-status-pairs
		  for i = 0 then (1+ i)
		  for index = (car index-status-pair)
		  for status = (cdr index-status-pair) collect
		  (cons (elt requests index) status))
	    (loop for r in remaining-indices collect (elt requests r)))))

(defun mpi-wait-some2 (requests)
  (mpi-wait/test-some2 requests :wait))

(defun mpi-test-some2 (requests)
  (mpi-wait/test-some2 requests :test))

(defun mpi-wait-some (requests)
  (mpi-wait/test-some requests :wait))

(defun mpi-test-some (requests)
  (mpi-wait/test-some requests :test))


(defun mpi-receive-string-nonblocking (source &key (tag +default-tag+) (buf-size-bytes *mpi-string-buf-size*))
  "receive string.
   returns a nonblocking request object
   INEFFICIENT - shouldn't allocate buffer every time."
  (declare (type (unsigned-byte 32) source tag))
  (let ((buf (cffi:foreign-alloc :char :count buf-size-bytes))
	(request (cffi:foreign-alloc 'MPI_Request)))
    (call-mpi (MPI_Irecv buf buf-size-bytes :MPI_CHAR source tag :MPI_COMM_WORLD request))
;    (make-request :mpi-request request :buf buf)))
    (make-request :mpi-request request :buf buf :count buf-size-bytes)))

(defun mpi-probe (source tag &key (base-type nil)(comm :MPI_COMM_WORLD)(blocking t))
  "Performs a (blocking or nonblocking) test for a message from a given source with given tag.
   The 'wildcards' MPI_ANY_SOURCE and MPI_ANY_TAG may be used to test for
   a message from any source or with any tag.
   ** Returns metadata for the message, not the message itself!
   The actual source and tag are stored in the status struct,
   so we will return status (if a probe succeeds), nil otherwise, or blocks"
  (cond (blocking
	 (cffi:with-foreign-objects ((mpi-status 'MPI_Status))
	   (MPI_Probe source tag comm (cffi:mem-aref mpi-status 'MPI_Status))
	   (MPI_Status->mpi-status mpi-status (lisp-type->mpi-type base-type))))
	(t ; non-blocking
	 (cffi:with-foreign-objects ((mpi-status 'MPI_Status)
				     (flag :int))
	   (MPI_Iprobe source tag comm flag (cffi:mem-aref mpi-status 'MPI_Status))
	   (if (= 0 (cffi:mem-aref flag :int)) nil (MPI_Status->mpi-status mpi-status (lisp-type->mpi-type base-type)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collective Operations
;;;


(defun mpi-broadcast (data &key (root 0)(comm :MPI_COMM_WORLD))
  "Broadcasts data from the process with rank root to all processes of the group (MPI_COMM_WORLD),
   itself included. It is called by all members of group using the same arguments for comm, root. On return, the contents of root's communication buffer has been copied to all processes.
   [** This is a specialized version of the MPI standard function MPI_BCAST (see http://www.mpi-forum.org/docs/mpi-11-html/node67.html)]


   Assume: if data is basic, then at the time of the call, it is set to the same type basic object at all procs.
           if data is simple-array (or string), then the types AND ARRAY COUNTS are the same at all procs -> will write into the provided array, and return a reference to it!

  "
  (let* ((metadata (match-type data))
	 (base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec->id metadata))
	 (cffi-type (typespec-cffi-type base-typespec))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)))
    (assert (and metadata base-typespec meta-id))
    (assert data) ; data needs to be the same type and count at all procs. Can't pass in nil!
    (tracep *trace1* t "Bcast: lisp-type=~a, count=~a, base-type=~a ~%" (obj-tspec-type metadata)  count base-typespec)
    (when (= +string+ meta-id)
      (cffi:with-foreign-pointer (buf count)
	(when (= root (mpi-comm-rank)) ; at the root, fill the buffer witht the string to be sent. At other nodes, don't bother filling the buffer
	  (cffi:with-foreign-string (cs data)
	    (loop for i from 0 below count do
		  (setf (cffi:mem-aref buf :char i) (cffi:mem-aref cs :char i)))))
	(call-mpi (MPI_Bcast buf count :MPI_CHAR root comm))
	;;(loop for i from 0 below count do (setf (aref data i) (cffi:mem-aref buf :char i)))
	;; XXX Inefficient, temporary hack -- I allocate a new lisp string with this foreign-string-to-lisp call
	(setf data (cffi:foreign-string-to-lisp buf)) ;; how can I replace this with something that overwrites data?
	(return-from mpi-broadcast data)))

    (cffi:with-foreign-object (buf (typespec-cffi-type base-typespec) (obj-tspec-count metadata))
      (cond ((= +simple-array+ meta-id)
	     (loop for i from 0 below count do
		   (setf (cffi:mem-aref buf cffi-type i)(aref data i)))
	     (call-mpi (MPI_Bcast buf count mpi-type  root comm))
	     (loop for i from 0 below count do
		   (setf (aref data i) (cffi:mem-aref buf cffi-type i)))
	     (return-from mpi-broadcast data) ; overwrites and returns the input array
	     )
	    ((= +base-object+ meta-id)
	     (setf (cffi:mem-aref buf cffi-type) data)
	     (call-mpi (MPI_Bcast buf  count mpi-type  root comm))
	     (return-from mpi-broadcast (cffi:mem-aref buf cffi-type))))
      )))

(defun mpi-broadcast-auto (data &key (root 0))
  "Broadcasts data from root to all processors. All processors in the group should call MPI-BROADCAST-STRING

   Returns the value of the Broadcasted data
   *** Broadcast is NOT the same as as send! It can be useful to synchronize an environment
   All processes must prepare a buffer to receive the broadcast
   e.g., (let ((r (mpi-broadcast-string (write-to-string (mpi-comm-rank))) :root 0))))
    would result in r on all nodes being assigned 0 (the rank of the root node, which is 0
   INEFFICIENT - first broadcasts metadata, then the actual payload.
   "
  (declare (optimize (speed 0) (debug 3)))
  (tracep *trace1* t "mpi-broadcast ~a [root=~a]~%"  data root)
  (let ((metadata nil) ; used only by root
	(typespec-id nil)
	(count 0)
	(meta-id nil))
    (when (= root (mpi-comm-rank));; compute metadata
      (setf metadata (match-type data)))

    ;; broadcast the type and count of data as a 3-element array
    (cffi:with-foreign-object (metadata-array :int 3)
      (when (= root (mpi-comm-rank))
	(tracep *trace1* t "mpi-broadcast-auto: metadata=~a~%" metadata)
	(setf (cffi:mem-aref metadata-array :int 0 ) (typespec-id (obj-tspec-base-typespec metadata)))
	(setf (cffi:mem-aref metadata-array :int 1 ) (obj-tspec-count metadata))
	(setf (cffi:mem-aref metadata-array :int 2 ) (obj-tspec->id metadata)))

      (call-mpi (MPI_Bcast metadata-array 3 :MPI_INT root :MPI_COMM_WORLD))
      (tracep *trace1* t "received broadcast datatype ~a, count=~a, meta-id=~a ~%" (cffi:mem-aref metadata-array :int 0)(cffi:mem-aref metadata-array :int 1)(cffi:mem-aref metadata-array :int 2))

      (setf typespec-id (cffi:mem-aref metadata-array :int 0)
	    count (cffi:mem-aref metadata-array :int 1)
	    meta-id (cffi:mem-aref metadata-array :int 2)))

    ;; broadcast the actual data payload
    (cond ((or (= +converted-object+ meta-id)(= +string+ meta-id))
	   (cffi:with-foreign-pointer (buf count)
	     (when (= root (mpi-comm-rank)) ; at the root, fill the buffer witht the string to be sent. At other nodes, don't bother filling the buffer
	       (cffi:with-foreign-string (cs (if (= +converted-object+ meta-id) (obj-tspec-converted-obj metadata) data))
		 (loop for i from 0 below count do
		       (setf (cffi:mem-aref buf :char i) (cffi:mem-aref cs :char i)))))
	     (call-mpi (MPI_Bcast buf count :MPI_CHAR root :MPI_COMM_WORLD))
	     (if (= +converted-object+ meta-id)
		 (read-from-string (cffi:foreign-string-to-lisp buf :count count))
		 (cffi:foreign-string-to-lisp buf :count count))))
	  ((= +simple-array+ meta-id)
	   (multiple-value-bind (base-type base-type-bytes mpi-type cffi-type)
	       (get-typespec-by-index typespec-id)
	     (declare (ignore base-type-bytes))
	     (assert base-type)
	     (cffi:with-foreign-object (buf cffi-type count)
	       (when (= root (mpi-comm-rank))
		 (loop for i from 0 below count do
		       (setf (cffi:mem-aref buf cffi-type i)(aref data i))))
	       (call-mpi (MPI_Bcast buf count mpi-type root :MPI_COMM_WORLD))
	       (make-array count :element-type base-type :initial-contents
			   (loop for i from 0 below count collect (cffi:mem-aref buf cffi-type i))))))
	  ((= +base-object+ meta-id)
	   (multiple-value-bind (base-type base-type-bytes mpi-type cffi-type)
	     (get-typespec-by-index typespec-id)
	     (assert base-type)
	     (cffi:with-foreign-pointer (buf base-type-bytes)
	       (when (= root (mpi-comm-rank))
		 (setf (cffi:mem-aref buf cffi-type) data))
	       (call-mpi (MPI_Bcast buf 1 mpi-type root :MPI_COMM_WORLD))
	       (cffi:mem-aref buf cffi-type))))
	  )))



(defun mpi-reduce (data op &key (root 0) (comm :MPI_COMM_WORLD)(allreduce nil))
  "Calls MPI_Reduce with operator op on obj, which is count instances of datatype.

   [from https://computing.llnl.gov/tutorials/mpi/man/MPI_Reduce.txt]
   Applies a reduction operation to the vector sendbuf over
   the set of tasks specified by comm and places the result in recvbuf on root.

   The input buffer and the output buffer have the same number of elements
   with the same type. The arguments sendbuf, count, and datatype define the
   send or input buffer. The arguments recvbuf, count and datatype define the
   output buffer. MPI_REDUCE is called by all group members using the same
   arguments for count, datatype, op, and root. If a sequence of elements is
   provided to a task, the reduction operation is executed element-wise on
   each entry of the sequence. Here's an example. If the operation is MPI_MAX
   and the send buffer contains two elements that are floating point numbers
   (count = 2 and datatype = MPI_FLOAT), recvbuf(1) = global max(sendbuf(1))
   and recvbuf(2) = global max(sendbuf(2)).

   ** Returns a NEW object (base object or array)"

  (let* ((metadata (match-type data))
	 (base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec->id metadata))
	 (cffi-type (typespec-cffi-type base-typespec))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)))
    (assert (and metadata base-typespec meta-id))
    (assert data) ; data needs to be the same type and count at all procs. Can't pass in nil!
    (tracep *trace1* t "Reduce: lisp-type=~a, count=~a, base-type=~a ~%" (obj-tspec-type metadata)  count base-typespec)
    (assert (/= +string+ meta-id))
    ;; This function will only handle simple arrays of base objects!
    (assert (find (typespec-lisp-type base-typespec) '(single-float double-float fixnum (signed-byte 32))))
    (cffi:with-foreign-objects ((sendbuf (typespec-cffi-type base-typespec) (obj-tspec-count metadata))
				(recvbuf (typespec-cffi-type base-typespec) (obj-tspec-count metadata)))
      (cond ((= +simple-array+ meta-id)
	     (loop for i from 0 below count do
		   (setf (cffi:mem-aref sendbuf cffi-type i)(aref data i)))
	     (if allreduce
		 (call-mpi (MPI_Allreduce sendbuf recvbuf count mpi-type  op comm))
		 (call-mpi (MPI_Reduce sendbuf recvbuf count mpi-type  op root comm)))
	     (let ((newdata (make-array count :element-type (typespec-lisp-type base-typespec))))
	       (loop for i from 0 below count do
		     (setf (aref newdata i) (cffi:mem-aref recvbuf cffi-type i)))
	       (return-from mpi-reduce newdata))) ; returns a new array
	    ((= +base-object+ meta-id)
	     (setf (cffi:mem-aref sendbuf cffi-type) data)
	     (if allreduce
		 (call-mpi (MPI_Allreduce sendbuf recvbuf count mpi-type  op comm))
		 (call-mpi (MPI_Reduce sendbuf recvbuf count mpi-type  op root comm)))
	     (return-from mpi-reduce (cffi:mem-aref recvbuf cffi-type))))
      )))

(defun mpi-allreduce (data op &key (root 0) (comm :MPI_COMM_WORLD))
  "MPI_Allreduce
   Like mpi-reduce, except that the result of the reduction is sent to all processors, and not just the root"
  (mpi-reduce data op :root root :comm comm :allreduce t))



#|
what to do for a better scatter, using sctterv

Let c = # of items in data
    p = comm-size

most intuitive: mpi-scatter scatters c elements evenly among the p processes.
If (rem c p)=0, then no problem.
If (rem c p)>= 1, then what to do?
Depends on n:
If c/n > p, then don's send the remainder
e.g., p=4, d=10, n=2, assigns 2 to each proc, remainder of 2.
If c/n < p, then the last proc receives null data? (send buffer must be extended?)
e.g., p=4, d=10, n=3, assig

|#


(defun mpi-scatter-gather (data &key (root 0)(comm :MPI_COMM_WORLD)(scatter-gather nil)(all nil))
  "Distributes/collects messages from root to all procs.
   'all' indicates Allgather, when scatter-gather==gather
   Note that Scatter is restrictive: Requires contiguous data, uniform message size.
   Requires that the total count of the data to be scattered is evenly divisible by the # of procs.
   Assumes that 'data' input parameter is same type and size at every node
   If this is not the case, use MPI-SCATTERV.
   The underlying MPI_Scatter function is a bit more flexible.(e.g., allows different type/count mappings between sender and receiver)"
  (assert scatter-gather) ; must specify either scatter or gather
  (assert (is-simple-array data)) ; mpi-scatter only makes senses for simple-arrays
  (when (equal scatter-gather :scatter)
    (assert (= 0 (rem (length data) (mpi-comm-size))))) ; mpi-scatter only makes sense when data is evenly divisible by # of procs.
  (let* ((metadata (match-type data))
	 (base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec->id metadata))
	 (cffi-type (typespec-cffi-type base-typespec))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)) ; this is the size of 'data'
	 ;;(sendcount (/ count (mpi-comm-size)))); # of elements sent to each proc
	 (sendcount (case scatter-gather
		      (:scatter (/ count (mpi-comm-size))); # of elements sent to each proc
		      (:gather  count))) ;# of elements sent from each proc to root
	 (recvcount (case scatter-gather
		      (:scatter sendcount);# of elements received by each proc
		      (:gather  count))) ;# of elements received by root from each proc
	 (sendbuf-count (case scatter-gather
			  (:scatter count)
			  (:gather sendcount)))
	 (recvbuf-count (case scatter-gather
			  (:scatter recvcount)
			  (:gather (* (mpi-comm-size) sendcount))))
	 )
    (assert (and metadata base-typespec meta-id))
    (assert data) ; data needs to be the same type and count at all procs. Can't pass in nil!
    (tracep *trace1* t "Scatter/gather: lisp-type=~a, count=~a, sendcount=~a, recvcount=~a, sendbuf-count=~a, recvbuf-count=~a~%  base-type=~a ~%"
	    (obj-tspec-type metadata)  count sendcount recvcount sendbuf-count recvbuf-count base-typespec)
    (when (= +string+ meta-id)
      (cffi:with-foreign-pointer (sendbuf sendbuf-count)
	(cffi:with-foreign-pointer (recvbuf recvbuf-count)
	  (cffi:with-foreign-string (cs data)
	    (loop for i from 0 below count do
		  (setf (cffi:mem-aref sendbuf :char i) (cffi:mem-aref cs :char i))))
	  (case scatter-gather
	    (:scatter (call-mpi (MPI_Scatter sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm)))
	    (:gather (if all
			 (call-mpi (MPI_Allgather sendbuf sendcount mpi-type recvbuf recvcount mpi-type comm))
			 (call-mpi (MPI_Gather sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm)))))
	  (return-from mpi-scatter-gather (cffi:foreign-string-to-lisp recvbuf))))) ;; should I replace this with something that overwrites data?

    (cffi:with-foreign-objects  ((sendbuf (typespec-cffi-type base-typespec) sendbuf-count)
				 (recvbuf (typespec-cffi-type base-typespec) recvbuf-count))
      (loop for i from 0 below sendbuf-count do
	    (setf (cffi:mem-aref sendbuf cffi-type i)(aref data i)))
      (case scatter-gather
	(:scatter (call-mpi (MPI_Scatter sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm)))
	(:gather (if all
		     (call-mpi (MPI_Allgather sendbuf sendcount mpi-type recvbuf recvcount mpi-type comm))
		     (call-mpi (MPI_Gather sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm)))))
      (return-from mpi-scatter-gather (make-array recvbuf-count :element-type (typespec-lisp-type base-typespec) :initial-contents
						  (loop for i from 0 below recvbuf-count
							collect (cffi:mem-aref recvbuf cffi-type i)))))))

(defun mpi-scatter (data &key (root 0)(comm :MPI_COMM_WORLD))
  (mpi-scatter-gather data :root root :comm comm :scatter-gather :scatter))


(defun mpi-gather (data &key (root 0)(comm :MPI_COMM_WORLD))
  (mpi-scatter-gather data :root root :comm comm :scatter-gather :gather))

(defun mpi-allgather (data &key (comm :MPI_COMM_WORLD))
  (mpi-scatter-gather data :comm comm :scatter-gather :gather :all t))





