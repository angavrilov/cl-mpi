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

(defmacro aprog1 (form &body code)
  `(let ((it ,form))
     (prog1 it ,@code)))

(defmacro memoize ((key &key (test '#'eql)) &body code)
  (let ((kvar (gensym)) (hvar (gensym))
        (res (gensym)) (found (gensym)))
    `(let ((,kvar ,key)
           (,hvar (load-time-value (make-hash-table :test ,test))))
       (multiple-value-bind (,res ,found) (gethash ,kvar ,hvar)
         (if ,found ,res
             (setf (gethash ,kvar ,hvar) (progn ,@code)))))))

(defmacro formatp (stream format-string &rest rest)
  "For debugging CL-MPI.
   Like format, but attaches 'Proc #' and wtime to the output so that it's easier to understand
   where the messages are being generated"
  ;;(format t "format-string=~a, rest=~a~%" format-string rest)
  (let ((g-value (gensym)))
    `(progn (let ((,g-value (format ,stream ,(concatenate 'string "[~a :~,4f]: "  format-string) (mpi-comm-rank) (mpi-wtime) ,@rest)))
	      (force-output ,stream)
	      ,g-value))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment-related API
;;;

(defun mpi-init ()
  "This routine must be called before any other MPI routine.
It must be called at most once; subsequent calls are erroneous.
All MPI programs must contain a call to MPI-INIT; this routine
must be called before any other MPI routine, apart from
MPI-INITIALIZED, is called.
See MPI_INIT docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node151.html"
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
	  (%mpi-init argc argvp))
	(cffi:foreign-free argv)))))

(defmacro with-mpi (&body body)
  "Executes body in an MPI environment (initializes and finalizes MPI before/after body)"
  `(progn
     (mpi-init)
     (unwind-protect (progn ,@body)
       (mpi-finalize))))

(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it
was called at the moment of the call.
  The name is a character string for maximum flexibility.
From this value it must be possible to identify a specific
piece of hardware;
  Possible values include 'processor 9 in rack 4 of mpp.cs.org'
and '231' (where 231 is the actual processor number in the
running homogeneous system).
See MPI_GET_PROCESSOR_NAME docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node143.html"
  (cffi:with-foreign-object (namelen :int) ;length of name returned by MPI call
    (cffi:with-foreign-pointer (processor-name MPI_MAX_PROCESSOR_NAME)
      (%mpi-get-processor-name processor-name namelen)
      (tracep *trace1* t "namelen=~a, processor-name=~a, namelen as lisp=~a~%" namelen processor-name (cffi:mem-aref namelen :int))
      (cffi:foreign-string-to-lisp processor-name :count (cffi:mem-aref namelen :int)))))

(defun mpi-wtime ()
  "Returns a (double) floating-point number of seconds,
representing elapsed wall-clock time since some time in
the past.
  The 'time in the past' is guaranteed not to change during
the life of the process.
  The user is responsible for converting large numbers
of seconds to other units if they are preferred.
  This function is portable (it returns seconds, not 'ticks'),
it allows high-resolution, and carries no unnecessary baggage.
  The times returned are local to the node that called them.
There is no requirement that different nodes return 'the same time.'
See MPI_WITME docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node150.html"
  ;;TODO (But see also the discussion of MPI_WTIME_IS_GLOBAL).
  ;;Does not return error, so don't wrap!
  (MPI_Wtime))

(defun mpi-wtick ()
  "Returns the resolution of MPI-WTIME in seconds.
That is, it returns, as a double precision value,
the number of seconds between successive clock ticks.
For example, if the clock is implemented by the hardware
as a counter that is incremented every millisecond,
the value returned by MPI-WTICK should be 0.001
See MPI_WTICK docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node150.html"
  ;; does not return error, so don't wrap!
  (MPI_Wtick))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Metadata manipulation
;;;

(defparameter *lisp-cffi-mpi-conversions*
  (loop for (lisp-type size mpi-type cffi-type id unsafe-p)
     in '(((signed-byte 8) 1 :MPI_CHAR :int8)
          ((unsigned-byte 8) 1 :MPI_UNSIGNED_CHAR :uint8)
          ((signed-byte 16) 2 :MPI_SHORT :int16)
          ((unsigned-byte 16) 2 :MPI_UNSIGNED_SHORT :uint16)
          ((signed-byte 32) 4 :MPI_INT :int32)
          ((unsigned-byte 32) 4 :MPI_UNSIGNED :uint32)
          ((signed-byte 64) 8 :MPI_LONG_LONG :int64)
          ((unsigned-byte 64) 8 :MPI_UNSIGNED_LONG_LONG :uint64)
          (single-float 4 :MPI_FLOAT :float)
          (double-float 8 :MPI_DOUBLE :double)
          (character 1 :MPI_UNSIGNED_CHAR :unsigned-char nil t)
          #+(or x86-64 x86_64)
          (fixnum 8 :MPI_LONG_LONG :int64 100 t)
          #-(or x86-64 x86_64)
          (fixnum 4 :MPI_INT :int32 101 t))
     and def-id from 0
     collect (make-typespec :lisp-type lisp-type
                            :size size
                            :mpi-type mpi-type
                            :cffi-type cffi-type
                            :id (or id def-id)
                            :unsafe-p unsafe-p))
  "basic mappings between lisp types, cffi tyes, and mpi types")

(defun explode-typespec (s)
  (when s
    (values (typespec-lisp-type s)
            (typespec-size s)
            (typespec-mpi-type s)
            (typespec-cffi-type s)
            (typespec-id s))))

(defconstant +converted-object+ 1)
(defconstant +simple-array+ 2)
(defconstant +string+ 3)
(defconstant +base-object+ 4)

(defconstant +metadata-tag+ 32767)

(defun get-typespec-by-type (lisp-type)
  (memoize (lisp-type :test #'equal)
    (find-if (lambda (type-spec)
               (and (subtypep lisp-type (typespec-lisp-type type-spec))
                    (subtypep (typespec-lisp-type type-spec) lisp-type)))
             *lisp-cffi-mpi-conversions*)))

(defun get-typespec-by-subtype (lisp-type)
  (memoize (lisp-type :test #'equal)
    (find-if (lambda (type-spec)
               (subtypep lisp-type (typespec-lisp-type type-spec)))
             *lisp-cffi-mpi-conversions*)))

(defun get-typespec-by-index (lisp-type-index)
  (memoize (lisp-type-index)
    (or (find lisp-type-index *lisp-cffi-mpi-conversions* :key #'typespec-id)
        (error "Invalid typespec ID: ~A" lisp-type-index))))

(defun to-string (d)
  "Converts d to a string which can be READ"
  (let ((*print-readably* t))
    (prin1-to-string d)))

(defun array-type-spec-p (typespec)
  (and (consp typespec)
       (subtypep typespec 'array)))

(defun get-scalar-typespec (lisp-type)
  (or (get-typespec-by-subtype lisp-type)
      (error "Unsupported lisp type: ~S" lisp-type)))

(defun lisp-type->mpi-type (lisp-type)
  (typespec-mpi-type (get-scalar-typespec lisp-type)))

(defun get-array-elt-typespec (base-type)
  (or (get-typespec-by-type base-type)
      (error "Unsupported array element type: ~S" base-type)))

(defun match-type (object &key (enable-default-conversion t))
  "Converts an object to an obj-tspec structure.
   If enable-default-conversion is t, object is possibly converted (e.g., to a string)"
  (let* ((object-type (type-of object))
         (base-typespec (get-typespec-by-subtype object-type)))
    (cond (base-typespec
	   (make-obj-tspec :id +base-object+ :type object-type :count 1
                           :base-typespec base-typespec))
	  ((stringp object)
	   (make-obj-tspec :id +string+ :type 'string :count (length object)
                           :base-typespec (get-typespec-by-subtype 'character)))
	  ((arrayp object)
	   (let* ((base-type (array-element-type object))
		  (base-typespec (get-array-elt-typespec base-type)))
	     (make-obj-tspec :id +simple-array+ :type 'simple-array :count (array-total-size object)
                             :base-typespec base-typespec)))
	  ;; generic conversion to READable string for objects which
          ;; are not basic or specialized arrays
	  (enable-default-conversion
	   (let ((obj-string (to-string object)))
             (make-obj-tspec :id +converted-object+ :type 'string :count (length obj-string)
                             :base-typespec (get-typespec-by-subtype 'character)
                             :converted-obj obj-string)))
          (t
           (error "Unsupported MPI object type: ~S" object-type)))))

(defun match-type-spec (type count)
  (cond ((subtypep type 'string)
         (make-obj-tspec :id +string+ :type 'string :count count
                         :base-typespec (get-typespec-by-subtype 'character)))
        ((subtypep type 'array)
         (assert (consp type))
         (let* ((elt-type (upgraded-array-element-type (second type)))
                (base-typespec (get-array-elt-typespec elt-type)))
           (make-obj-tspec :id +simple-array+ :type 'simple-array :count count
                           :base-typespec base-typespec)))
        (t
         (assert (= count 1)) ; TODO: convert count>1 to array?
         (let* ((base-typespec (get-scalar-typespec type)))
           (make-obj-tspec :id +base-object+ :type type :count 1
                           :base-typespec base-typespec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array data access
;;;

#+ecl
(defun %get-array-data-ptr (array)
  (check-type array array)
  (ffi:c-inline (array) (:object) :object
    "ecl_make_pointer((#0)->array.self.t)"
    :one-liner t))

#+(or ecl sbcl)
(defmacro with-pointer-to-array-data ((pointer array elt-typespec) &body code)
  "Binds a pointer to the array contents. The elt-typespec _must_ match the real element type."
  (declare (ignorable elt-typespec))
  #+sbcl
  (let ((svec (gensym)) (sstart (gensym)) (send (gensym)))
    `(sb-kernel:with-array-data ((,svec ,array) (,sstart 0) (,send))
       (declare (ignore ,send))
       (sb-sys:with-pinned-objects (,svec)
         (let ((,pointer (sb-sys:sap+ (sb-sys:vector-sap ,svec)
                                      (* ,sstart (typespec-size ,elt-typespec)))))
           ,@code))))
  #+ecl
  `(let ((,pointer (%get-array-data-ptr ,array)))
     ,@code))

#+(or ecl sbcl)
(cffi:defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (count :unsigned-int))

(defun copy-cffi-to-array (array buffer typespec count &key force-safe)
  "Copy data from a buffer ta an array. The typespec _must_ match the real element type."
  (cond #+(or ecl sbcl)
        ((not (or force-safe (typespec-unsafe-p typespec)))
         (with-pointer-to-array-data (ptr array typespec)
           (memcpy ptr buffer (* count (typespec-size typespec)))))
        (t
         (let ((cffi-type (typespec-cffi-type typespec)))
           (loop for i from 0 below count
              do (setf (row-major-aref array i) (cffi:mem-aref buffer cffi-type i)))))))

(defun copy-cffi-from-array (array buffer typespec count &key force-safe)
  "Copy data from an array to a buffer. The typespec _must_ match the real element type."
  (cond #+(or ecl sbcl)
        ((not (or force-safe (typespec-unsafe-p typespec)))
         (with-pointer-to-array-data (ptr array typespec)
           (memcpy buffer ptr (* count (typespec-size typespec)))))
        (t
         (let ((cffi-type (typespec-cffi-type typespec)))
           (loop for i from 0 below count
              do (setf (cffi:mem-aref buffer cffi-type i) (row-major-aref array i)))))))

(defmacro with-array-data-copy ((buffer array elt-typespec count &key (in t) out (dealloc t) force-safe)
                                &body code)
  "Allocates a foreign buffer and moves data in and/or out of it."
  (let* ((stypespec (gensym)) (sarray (gensym)) (scount (gensym)))
    `(let ((,sarray ,array)
           (,stypespec ,elt-typespec)
           (,scount ,count))
       (,@(if dealloc
              `(cffi:with-foreign-object (,buffer (typespec-cffi-type ,stypespec) ,scount))
              `(let ((,buffer (cffi:foreign-alloc :char :count (1+ count))))))
          ,@(if in `((copy-cffi-from-array ,sarray ,buffer ,stypespec ,scount
                                           ,@(if force-safe '(:force-safe t)))))
          (multiple-value-prog1 (progn ,@code)
            ,@(if out `((copy-cffi-to-array ,sarray ,buffer ,stypespec ,scount
                                            ,@(if force-safe '(:force-safe t))))))))))

(defmacro with-array-data-access ((buffer array elt-typespec count &key (in t) out handle-unsafe) &body code)
  "Provides reference to the array data by either allocating a buffer, or direct access."
  (declare (ignorable in out count handle-unsafe))
  #+(or ecl sbcl)
  (let ((body
         `(with-pointer-to-array-data (,buffer ,array ,elt-typespec)
            ,@code)))
    (if handle-unsafe
        `(if (typespec-unsafe-p ,elt-typespec)
             (with-array-data-copy (,buffer ,array ,elt-typespec ,count :in ,in :out ,out)
               ,@code)
             ,body)
        `(progn
           (assert (not (typespec-unsafe-p ,elt-typespec)))
           ,body)))
  #-(or ecl sbcl)
  `(with-array-data-copy (,buffer ,array ,elt-typespec ,count :in ,in :out ,out)
     ,@code))

(defmacro with-scalar-data-copy ((buffer name elt-typespec &key init-with) &body code)
  "Allocates a scalar foreign object buffer with appropriate value init and/or retrieval."
  (let* ((stypespec (gensym)) (ctype (gensym)))
    `(let* ((,stypespec ,elt-typespec)
            (,ctype (typespec-cffi-type ,stypespec)))
       (cffi:with-foreign-object (,buffer ,ctype)
         (symbol-macrolet ((,name (cffi:mem-ref ,buffer ,ctype)))
           ,@(if init-with `((setf ,name ,init-with)))
           ,@code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defparameter *message-buffer* nil "The buffer associated with this process")

(defun mpi-buffer-attach (buf-size-bytes)
  "Provides to MPI a buffer in the user's memory to be used for
buffering outgoing messages. The buffer is used only by messages
sent in buffered mode. Only one buffer can be attached to a process
at a time.
  MPI-BUFFER-ATTACH attaches a NEWLY ALLOCATED system message buffer
ao this process.
  Assigns *message-buffer* to the CFFI pointer to this new buffer.
  Also returns the CFFI pointer to this buffer
See MPI_BUFFER_ATTACH and MPI_BUFFER_DETACH docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node42.html"
  (assert (null *message-buffer*)) ; only one buffer can be attached to a process
  (let ((p (cffi:foreign-alloc :char :count buf-size-bytes)))
    (%mpi-buffer-attach p buf-size-bytes)
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
   See MPI_BUFFER_ATTACH and MPI_BUFFER_DETACH docs at:
     http://www.mpi-forum.org/docs/mpi-11-html/node42.html"
  (assert *message-buffer*)
  (multiple-value-bind (old-ptr old-size) (%mpi-buffer-detach)
    (declare (ignore old-ptr))
    (setf *message-buffer* nil)
    ;; (cffi:foreign-free *message-buffer*)
    #+nil(formatp t "deallocated-buf-address=~a, deallocated ~a bytes~%"
                  deallocated-buf-address (cffi:mem-aref c-deallocated-bytes :int))
    old-size))

(defmacro with-buffer (buf-size &body body)
  "Creates a new system message buffer with size buf-size, performs
the body, then detaches and deallocates the buffer.
See MPI_BUFFER_ATTACH and MPI_BUFFER_DETACH docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node42.html]"
  `(progn
     (mpi-buffer-attach ,buf-size)
     (unwind-protect
          (progn ,@body)
       (mpi-buffer-detach))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic point-to-point operations
;;;

(defun %mpi-send-1 (buf count mpi-type destination tag comm mode)
  "A low-level wrapper around the MPI_Send, MPI_Ssend, MPI_Rsend, and MPI_Bsend functions"
  (case mode
    (:basic       (mpi-send-ptr/basic buf count mpi-type destination tag comm))
    (:synchronous (mpi-send-ptr/synchronous buf count mpi-type destination tag comm))
    (:ready       (mpi-send-ptr/ready buf count mpi-type destination tag comm))
    (:buffered    (mpi-send-ptr/buffered buf count mpi-type destination tag comm))))

(defun %mpi-isend-1 (buf count mpi-type destination tag comm mode)
  "A low-level wrapper around the MPI_ISend, MPI_ISsend, MPI_IRsend, and MPI_IBsend functions"
  (case mode
    (:basic       (mpi-isend-ptr/basic buf count mpi-type destination tag comm))
    (:synchronous (mpi-isend-ptr/synchronous buf count mpi-type destination tag comm))
    (:ready       (mpi-isend-ptr/ready buf count mpi-type destination tag comm))
    (:buffered    (mpi-isend-ptr/buffered buf count mpi-type destination tag comm))))

(defun %mpi-send-meta (data metadata destination tag comm mode)
  (let* ((base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec-id metadata))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)))
    (tracep *trace1* t "Send1: ~A~%" metadata)
    (cond ((or (= +string+ meta-id)
               (= +converted-object+ meta-id))
           (cffi:with-foreign-string
               (cs (or (obj-tspec-converted-obj metadata) data))
             (%mpi-send-1 cs count :MPI_CHAR destination tag comm mode)))
          ((= +simple-array+ meta-id)
           (with-array-data-access (ptr data base-typespec count :handle-unsafe t)
             (%mpi-send-1 ptr count mpi-type destination tag comm mode)))
          ((= +base-object+ meta-id)
           (with-scalar-data-copy (ptr value base-typespec :init-with data)
             (%mpi-send-1 ptr 1 mpi-type destination tag comm mode)))
          (t (assert nil)))))

(defun mpi-send (data destination &key (tag +default-tag+) (mode :basic) (comm :MPI_COMM_WORLD))
  "Blocking send fuction. Sends data to definition.
Tries to be smart about automatically packaging the data depending
on its type, and handles standard MPI types and CL objects.
See related docs on blocking send (MPI_SEND):
  http://www.mpi-forum.org/docs/mpi-11-html/node31.html,
And communication modes at:
  http://www.mpi-forum.org/docs/mpi-11-html/node40.html"
  (%mpi-send-meta data (match-type data :enable-default-conversion nil)
                  destination tag comm mode))

(defun %mpi-receive-meta (metadata source tag comm)
  (let* ((base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec-id metadata))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)))
    (cond ((= +string+ meta-id)
           (mpi-receive-string source :tag tag :buf-size-bytes (* 2 count)))
          ((= +simple-array+ meta-id)
           (let ((array (make-array count :element-type (typespec-lisp-type base-typespec))))
             (with-array-data-access (ptr array base-typespec count :in nil :out t :handle-unsafe t)
               (let ((status (mpi-receive-ptr ptr count mpi-type source tag comm)))
                 (tracep *trace1* t "received array, status=~a~%" status)
                 (values array status)))))
          ((= +base-object+ meta-id)
           (with-scalar-data-copy (ptr value base-typespec)
             (let ((status (mpi-receive-ptr ptr 1 mpi-type source tag comm)))
               (tracep *trace1* t "received scalar, status=~a~%" status)
               (values value status))))
          ((= +converted-object+ meta-id)
           (read-from-string
            (mpi-receive-string source :tag tag :comm comm :buf-size-bytes count)))
          (t (assert nil)))))

(defun mpi-receive (source type count &key (tag +default-tag+) (comm :MPI_COMM_WORLD))
  "Smart receiver protocol -- assumes that receiver knows count of
the data has an appropriate count buffer ready to receive. Intended
to matches call to mpi-send. Blocking receive function. Allocates
and returns a new object.
  Returns (values data status)
See MPI_RECV docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node34.html"
  (%mpi-receive-meta (match-type-spec type count) source tag comm))

(defun mpi-receive1 (source type count &key (tag +default-tag+) (comm :MPI_COMM_WORLD))
  "Smart receiver protocol -- assumes that receiver knows type of data to receive
   intended to matches call to mpi-send
   Blocking receive function.
   Returns (values data status)
  [See MPI_RECV docs at  http://www.mpi-forum.org/docs/mpi-11-html/node34.html]
  will allocate and return a new object.
  "
  (declare (ignore count))
  (let* ((base-type (cond ((subtypep type 'string)   'character)
                          ((array-type-spec-p type)  (second type))
                          (t                         type)))
	 (status (mpi-probe source tag :base-type base-type))
	 (count (status-count status)))
    (assert count)
    (tracep *trace1* t "mpi-receive1 probed: status=~a~%" status)
    (mpi-receive source type count :tag tag :comm comm)))

(defun mpi-send-string (str destination &key (tag +default-tag+) (mode :basic)
                        (blocking t) (comm :MPI_COMM_WORLD))
  "Send string to destination.
Mode is one of:  :basic  :buffered :synchronous
When blocking is true, this call returns only after the
application buffer in the sending task is free for reuse. Note that
this routine may be implemented differently on different systems. The
MPI standard permits the use of a system buffer but does not require
it. Some implementations may actually use a synchronous send (MPI_Ssend)
to implement the basic blocking send.
If blocking is false, then a non-blocking send operation is used.
In case of :basic mode, nothing is guaranteed about the state of the
receiver. If the receiver is not ready to read the data, then the data
may be stored in a system buffer. However, it *might* be unsafe to
assume anything about the size of the the system buffer, which might
overflow.
:synchronous guarantees that we block until the recipient
starts reading the data.
:buffered explicitly states that a buffer will be used. Furthermore,
the buffer that is used is allocated by the user using attach_buffer.
See related docs on blocking send (MPI_SEND):
  http://www.mpi-forum.org/docs/mpi-11-html/node31.html,
Communication modes at:
  http://www.mpi-forum.org/docs/mpi-11-html/node40.html"
  (check-type str string)
  (let ((count (length str)))
    (assert (< count *mpi-string-buf-size*))
    (cond (blocking
	   (cffi:with-foreign-string (c-str str)
	     (%mpi-send-1 c-str count :MPI_CHAR destination tag comm mode)))
	  (t ;non-blocking
           ;; need to add 1 to count (and also null-terminates the string)
	   (let ((buf (cffi:foreign-alloc :char :count (1+ count))))
             (cffi:lisp-string-to-foreign str buf (1+ count))
             (aprog1 (%mpi-isend-1 buf count :MPI_CHAR destination tag comm mode)
               (setf (request-status-cb it) #'%dealloc-request-buffer)
               (tracep *trace1* t "mpi-send-string generated request = ~a~%" it)))))))

(defun mpi-receive-string (source &key (tag +default-tag+)
                           (buf-size-bytes *mpi-string-buf-size*) (comm :MPI_COMM_WORLD))
  "Blocking receive string. Returns (values string count)"
  (cffi:with-foreign-pointer (buf buf-size-bytes)
    (let* ((status (mpi-receive-ptr buf buf-size-bytes :MPI_BYTE source tag comm))
           (count (status-count status)))
      (values (cffi:foreign-string-to-lisp buf :count count) count))))

(defun mpi-send-receive-string (send-str destination source
				&key (send-tag +default-tag+) (recv-tag +default-tag+)
				(comm :MPI_COMM_WORLD) (recv-buf-size-bytes *mpi-string-buf-size*))
  "Blocking send-receive operation.
   Returns (values received-string size-of-received-message)"
  (cffi:with-foreign-string (c-send-str send-str)
    (cffi:with-foreign-pointer (recv-buf recv-buf-size-bytes)
      (let* ((status (mpi-sendrecv-ptr
                      c-send-str (length send-str) :MPI_CHAR destination send-tag
                      recv-buf recv-buf-size-bytes :MPI_CHAR source recv-tag
                      comm))
             (count (status-count status)))
        (values (cffi:foreign-string-to-lisp recv-buf :count count) count)))))

(defmacro with-foreign-metadata ((ptr size-var metadata &key (in t) out) &body code)
  `(cffi:with-foreign-object (,ptr :int 3)
     ,@(if (and metadata in)
           `((,@(if (eq in t) '(progn) `(when ,in))
                (setf (cffi:mem-aref ,ptr :int 0) (typespec-id (obj-tspec-base-typespec ,metadata))
                      (cffi:mem-aref ,ptr :int 1) (obj-tspec-count ,metadata)
                      (cffi:mem-aref ,ptr :int 2) (obj-tspec-id ,metadata)))))
     (let ((,size-var 3))
       ,@code)
     ,@(if out
           `((make-obj-tspec :id (cffi:mem-aref ,ptr :int 2)
                             :count (cffi:mem-aref ,ptr :int 1)
                             :base-typespec (get-typespec-by-index
                                             (cffi:mem-aref ,ptr :int 0)))))))

(defun mpi-send-auto (data destination &key (tag +default-tag+) (mode :basic) (comm :MPI_COMM_WORLD))
  "Sends data to destination. Data is any Lisp object.
INEFFICIENT: First sends metadata (type, count) which is necessary
to set up the second send, which is the actual data payload."
  (let ((metadata (match-type data)))
    (tracep *trace1* t "send-auto metadata = ~A~%" metadata)
    (with-foreign-metadata (ptr size metadata)
      (%mpi-send-1 ptr size :MPI_INT destination +metadata-tag+ comm mode))
    (%mpi-send-meta data metadata destination tag comm mode)))

(defun mpi-receive-auto (source &key (tag +default-tag+) (comm :MPI_COMM_WORLD))
  "Receives data from the source.
INEFFICIENT: First receives metadata (type, count) which is necessary
to set up the second receive call, which is the actual data payload."
  (let ((metadata (with-foreign-metadata (ptr size nil :out t)
                    (mpi-receive-ptr ptr size :MPI_INT source +metadata-tag+ comm))))
    (tracep *trace1* t "receive-auto metadata = ~A~%" metadata)
    (%mpi-receive-meta metadata source tag comm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; non-blocking communications
;;;

(defun %dealloc-request-buffer (request status)
  "A request callback that simply deallocates its buffer."
  (declare (ignore status))
  (assert (request-deallocated-p request))
  (when (request-buf request)
    (cffi:foreign-free (request-buf request))
    (setf (request-buf request) nil))
  (setf (request-status-cb request) nil))

(defun request-get-string (request)
  (unless (request-deallocated-p request)
    (mpi-wait request))
  (assert (stringp (request-result request)))
  (request-result request))

(defun %complete-string-receive (request status)
  "A request callback for nonblocking string receive."
  (let ((buf (request-buf request))
        (count (status-count status)))
    (assert (and buf count))
    (setf (request-result request)
          (cffi:foreign-string-to-lisp buf :count count))
    (%dealloc-request-buffer request status)))

(defun mpi-wait-any2 (requests)
  "Returns (values received_request status requests_minus_completed_request)
   This version is so that we don't have to keep track of request indices."
  (multiple-value-bind (completed-index status)
      (mpi-wait-any requests)
    (values (elt requests completed-index) status (remove (elt requests completed-index) requests))))

(defun mpi-wait/test-some (requests mode)
  "Block or just check if some requests are complete.
Returns an alist of completed indexes & statuses."
  (multiple-value-bind (num-done indexes statuses)
      (ecase mode
	(:wait (%mpi-wait-some requests))
	(:test (%mpi-test-some requests)))
    (tracep *trace1* t "num-done=~a~%" num-done)
    (when (> num-done 0)
      (map 'list #'cons indexes statuses))))

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
  "Initiate a nonblocking string receive. Returns a nonblocking request object."
  (let ((buf (cffi:foreign-alloc :char :count buf-size-bytes)))
    (aprog1 (mpi-ireceive-ptr buf buf-size-bytes :MPI_CHAR source tag :MPI_COMM_WORLD)
      (setf (request-status-cb it) #'%complete-string-receive))))

(defun mpi-probe (source tag &key (base-type nil)(comm :MPI_COMM_WORLD)(blocking t))
  "Performs a (blocking or nonblocking) test for a message from a given source with given tag.
   The 'wildcards' MPI_ANY_SOURCE and MPI_ANY_TAG may be used to test for
   a message from any source or with any tag.
   ** Returns metadata for the message, not the message itself!
   The actual source and tag are stored in the status struct,
   so we will return status (if a probe succeeds), nil otherwise, or blocks"
  (cond (blocking
         (%mpi-probe (lisp-type->mpi-type base-type) source tag comm))
	(t ; non-blocking
         (%mpi-iprobe (lisp-type->mpi-type base-type) source tag comm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collective Operations
;;;

(defun %mpi-broadcast-meta (data metadata root comm buf-size-bytes)
  (let* ((base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec-id metadata))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (count (obj-tspec-count metadata)))
    (tracep *trace1* t "Broadcast: ~A~%" metadata)
    (cond ((or (= +string+ meta-id)
               (= +converted-object+ meta-id))
           (cffi:with-foreign-pointer (buf buf-size-bytes)
             (when (= root (mpi-comm-rank))
               (assert (< count buf-size-bytes))
               (cffi:with-foreign-string
                   (cs (or (obj-tspec-converted-obj metadata) data))
                 (memcpy buf cs count))
               (setf (cffi:mem-ref buf :int8 count) 0))
             (mpi-broadcast-ptr buf buf-size-bytes :MPI_CHAR root comm)
             (let ((rv (cffi:foreign-string-to-lisp buf)))
               (if (= +converted-object+ meta-id)
                   (read-from-string rv)
                   rv))))
          ((= +simple-array+ meta-id)
           (with-array-data-access (ptr data base-typespec count :out t :handle-unsafe t)
             (mpi-broadcast-ptr ptr count mpi-type root comm)))
          ((= +base-object+ meta-id)
           (with-scalar-data-copy (ptr value base-typespec :init-with data)
             (mpi-broadcast-ptr ptr 1 mpi-type root comm)
             value))
          (t (assert nil)))))

(defun mpi-broadcast (data &key (root 0) (comm :MPI_COMM_WORLD)
                      (buf-size-bytes *mpi-string-buf-size*))
  "Broadcasts data from the process with rank root to all processes
of the group (MPI_COMM_WORLD), itself included. It is called by all
members of group using the same arguments for comm, root. On return,
the contents of root's communication buffer has been copied to all
processes.
  This is a specialized version of the MPI standard function
MPI_BCAST: see http://www.mpi-forum.org/docs/mpi-11-html/node67.html.
  Assume: if data is basic, then at the time of the call, it is set
to the same type basic object at all procs.
  If data is simple-array (or string), then the types AND ARRAY
COUNTS are the same at all procs -> will write into the provided
array, and return a reference to it!"
  (let ((metadata (match-type data :enable-default-conversion nil)))
    (%mpi-broadcast-meta data metadata root comm buf-size-bytes)))

(defun mpi-broadcast-auto (data &key (root 0) (comm :MPI_COMM_WORLD))
  "Broadcasts data from root to all processors. All processors in the group
should call MPI-BROADCAST-AUTO. Returns the value of the broadcasted data.
INEFFICIENT - first broadcasts metadata, then the actual payload."
  (tracep *trace1* t "mpi-broadcast ~a [root=~a]~%"  data root)
  (let* ((root?         (= root (mpi-comm-rank)))
         (init-metadata (if root? (match-type data)))
         (new-metadata (with-foreign-metadata (ptr size init-metadata :in root? :out t)
                         (mpi-broadcast-ptr ptr size :MPI_INT root comm)))
         (metadata (if root? init-metadata new-metadata)))
    (%mpi-broadcast-meta data metadata root comm (1+ (obj-tspec-count metadata)))))

(defun mpi-reduce (data op &key (root 0) (comm :MPI_COMM_WORLD) (allreduce nil) into-array)
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

  (let* ((metadata (match-type data :enable-default-conversion nil))
	 (base-typespec (obj-tspec-base-typespec metadata))
         (cffi-type (typespec-cffi-type base-typespec))
	 (mpi-type (typespec-mpi-type base-typespec))
	 (meta-id (obj-tspec-id metadata))
	 (count (obj-tspec-count metadata))
         (return? (or allreduce (= root (mpi-comm-rank)))))
    (tracep *trace1* t "Reduce: ~a~%" metadata)
    (cond ((= +simple-array+ meta-id)
           (with-array-data-access (src-ptr data base-typespec count :handle-unsafe t)
             (if return?
                 (let* ((elt-type (typespec-lisp-type base-typespec))
                        (out-array (or into-array (make-array (array-dimensions data)
                                                              :element-type elt-type))))
                   (when into-array
                     (assert (and (= (array-total-size out-array) count)
                                  (equal (array-element-type out-array) elt-type)
                                  (not (eq data out-array)))))
                   (with-array-data-access (out-ptr out-array base-typespec count
                                                    :in nil :out t :handle-unsafe t)
                     (if allreduce
                         (mpi-all-reduce-ptr src-ptr out-ptr count mpi-type op comm)
                         (mpi-reduce-ptr src-ptr out-ptr count mpi-type op root comm)))
                   out-array)
                 (cffi:with-foreign-object (out-ptr cffi-type count)
                   (mpi-reduce-ptr src-ptr out-ptr count mpi-type op root comm)))))
          ((= +base-object+ meta-id)
           (with-scalar-data-copy (src-ptr src-value base-typespec :init-with data)
             (with-scalar-data-copy (out-ptr out-value base-typespec)
               (if allreduce
                   (mpi-all-reduce-ptr src-ptr out-ptr 1 mpi-type op comm)
                   (mpi-reduce-ptr src-ptr out-ptr 1 mpi-type op root comm))
               (when return? out-value))))
          (t
           (error "This function can only be used with scalars and arrays.")))))

(defun mpi-allreduce (data op &key (root 0) (comm :MPI_COMM_WORLD) into-array)
  "MPI_Allreduce
   Like mpi-reduce, except that the result of the reduction is sent to all processors, and not just the root"
  (mpi-reduce data op :root root :comm comm :allreduce t :into-array into-array))



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
  (assert (arrayp data)) ; mpi-scatter only makes senses for simple-arrays
  (when (equal scatter-gather :scatter)
    (assert (= 0 (rem (length data) (mpi-comm-size))))) ; mpi-scatter only makes sense when data is evenly divisible by # of procs.
  (let* ((metadata (match-type data :enable-default-conversion nil))
	 (base-typespec (obj-tspec-base-typespec metadata))
	 (meta-id (obj-tspec-id metadata))
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
    (tracep *trace1* t "Scatter/gather: lisp-type=~a, count=~a, sendcount=~a, recvcount=~a, sendbuf-count=~a, recvbuf-count=~a~%  base-type=~a ~%"
	    (obj-tspec-type metadata)  count sendcount recvcount sendbuf-count recvbuf-count base-typespec)
    (when (= +string+ meta-id)
      (cffi:with-foreign-pointer (sendbuf sendbuf-count)
	(cffi:with-foreign-pointer (recvbuf recvbuf-count)
	  (cffi:with-foreign-string (cs data)
	    (loop for i from 0 below count do
		  (setf (cffi:mem-aref sendbuf :char i) (cffi:mem-aref cs :char i))))
	  (case scatter-gather
	    (:scatter (mpi-scatter-ptr sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm))
	    (:gather (if all
			 (mpi-all-gather-ptr sendbuf sendcount mpi-type recvbuf recvcount mpi-type comm)
			 (mpi-gather-ptr sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm))))
	  (return-from mpi-scatter-gather (cffi:foreign-string-to-lisp recvbuf))))) ;; should I replace this with something that overwrites data?

    (cffi:with-foreign-objects  ((sendbuf (typespec-cffi-type base-typespec) sendbuf-count)
				 (recvbuf (typespec-cffi-type base-typespec) recvbuf-count))
      (loop for i from 0 below sendbuf-count
         do (setf (cffi:mem-aref sendbuf cffi-type i) (row-major-aref data i)))
      (case scatter-gather
	(:scatter (mpi-scatter-ptr sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm))
	(:gather (if all
		     (mpi-all-gather-ptr sendbuf sendcount mpi-type recvbuf recvcount mpi-type comm)
		     (mpi-gather-ptr sendbuf sendcount mpi-type recvbuf recvcount mpi-type root comm))))
      (return-from mpi-scatter-gather (make-array recvbuf-count :element-type (typespec-lisp-type base-typespec) :initial-contents
						  (loop for i from 0 below recvbuf-count
							collect (cffi:mem-aref recvbuf cffi-type i)))))))

(defun mpi-scatter (data &key (root 0)(comm :MPI_COMM_WORLD))
  (mpi-scatter-gather data :root root :comm comm :scatter-gather :scatter))


(defun mpi-gather (data &key (root 0)(comm :MPI_COMM_WORLD))
  (mpi-scatter-gather data :root root :comm comm :scatter-gather :gather))

(defun mpi-allgather (data &key (comm :MPI_COMM_WORLD))
  (mpi-scatter-gather data :comm comm :scatter-gather :gather :all t))





