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

#-ecl-mpicc-compile
(load-mpi-foreign-libraries)

#+ecl-mpicc-compile
(eval-when (:load-toplevel)
  (ffi:clines "#include <mpi.h>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants and enums for ECL
;;;

#+ecl-mpicc-compile
(macrolet ((include (&rest x) (declare (ignore x)))
           (in-package (&rest x) (declare (ignore x)))
           (ctype (&rest x) (declare (ignore x)))
           (cstruct (&rest x) (declare (ignore x)))
           (constantenum (name &rest items)
             `(eval-when (:compile-toplevel :execute)
                (setf (get ',name 'input-ffi-type) :object
                      (get ',name 'parsed-c-type) "int"
                      (get ',name 'emit-parser)
                      (lambda (rvar ivar)
                        (with-output-to-string (stream)
                          ,@(loop for ((name cname)) in items
                               collect `(format stream "if (~A == @~S) ~A = ~A;~%  else "
                                                ivar ',name rvar ,cname))
                          (format stream "~A = ecl_to_int(~A);" rvar ivar))))))
           (constant ((name cname))
             `(progn
                (eval-when (:load-toplevel)
                  (let ()
                    (defconstant ,name (ffi:c-inline () () :int ,cname :one-liner t))))
                (eval-when (:execute)
                  (defvar ,name #:INVALID-CONSTANT-VALUE)))))
  #.(with-open-file (stream (merge-pathnames #P"mpi-grovel.lisp"
                                             (or *compile-file-truename*
                                                 *load-truename*)))
      (list* 'progn
             (loop for form = (read stream nil :end-of-file nil)
                while (not (eq form :end-of-file))
                collect form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; API wrapper parameter specification syntax
;;;

#|
Parameter spec structure:

  (name ffi-type mode default)

Supported modes:

  :in (NIL) - ordinary input argument
  :out      - output argument; the C function receives
              a pointer to a temporary foreign object.
  :inout    - like out, but the temporary is initialized
              from a function input argument.
  :skip     - the parameter is not passed to the C function,
              but may be used by default expressions and so on.
  :aux      - the parameter value is always computed from default.
  :mutate   - for MPI_Request: :in, but the lisp object is
              destructively updated afterwards.

  :abort-flag - for :boolean; if not true, the function
                immediately returns before processing any
                mutate or output arguments. The flag
                itself is never returned.

Output parameters are returned as multiple values.
Specifying a default value for an input non-aux parameter
makes it and any following ones optional.

Special parameter names:

  seq/count   - an input parameter used by .../seq special
                types as the length of the sequence to
                allocate and marshal.
  out/count   - an output parameter that limits the length
                of output sequence parameters if present.

Special type handling:

  MPI_Status -  Output only; a lisp status structure is
                allocated and returned.
                The default value must specify how to get
                the operation datatype.
  MPI_Request - In output mode, a structure is allocated
                and returned. The default value is a parameter
                list of (buffer count datatype).
                Supports :mutate, in which case the input
                structure is modified to reflect changes
                in the handle value by the C call.

  MPI_Request/seq - mutate only; the parameter must be
                    a sequence of length seq/count,
                    and seq/count must be an existing
                    input or aux parameter.
  MPI_Status/seq  - a sequence of seq/count or out/count
                    statuses is computed and returned.
  int/seq         - a sequence of seq/count or out/count
                    ints is returned.

Additional "default" arguments to output parameters may
use the values of preceeding output parameters by referring
to their names.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structure interface
;;;

#+ecl-mpicc-compile
(progn
  (eval-when (:load-toplevel)
    (defvar *null-request-handle*
      (ffi:c-inline () () :object
        "ecl_make_pointer((void*)MPI_REQUEST_NULL)"
        :one-liner t)))

  (eval-when (:compile-toplevel :execute)
    (defvar *null-request-handle* nil))

  (defun request-deallocated-p (request)
    (equal (request-mpi-request request) *null-request-handle*)))

(define-api-call (%mpi-request-free "MPI_Request_free")
    ((request MPI_Request :mutate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment-related API
;;;

(define-api-call (mpi-initialized "MPI_Initialized")
    ((flag :boolean :out))
    "Returns true if MPI_INIT has been called and nil otherwise.
This routine may be used to determine whether MPI-INIT has
been called. It is the only routine that may be called before
MPI-INIT is called.
See MPI_INITIALIZED docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node151.html.")

(define-api-call (%mpi-init "MPI_Init")
    ((argc :pointer)
     (argv :pointer)))

(define-api-call (mpi-finalize "MPI_Finalize")
    ()
    "This routines cleans up all MPI state. Once this routine is
called, no MPI routine (even MPI-INIT) may be called. The user
must ensure that all pending communications involving a process
 completes before the process calls MPI-FINALIZE.
See MPI_FINALIZE docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node151.html")

(define-api-call (mpi-comm-rank "MPI_Comm_rank" :disabled 0)
    ((communicator MPI_Comm :in :MPI_COMM_WORLD)
     (myid :int :out))
    "Indicates the index of the current process within a communicator.
For MPI_COMM_WORLD, it indicates the global index of the process.
See MPI_COMM_RANK docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node101.html]")

(define-api-call (mpi-comm-size "MPI_Comm_size")
    ((communicator MPI_Comm :in :MPI_COMM_WORLD)
     (numprocs :int :out))
    "Indicates the number of processes involved in a communicator.
For MPI_COMM_WORLD, it indicates the total number of processes available
See MPI_COMM_SIZE docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node101.html]")

(define-api-call (mpi-abort "MPI_Abort")
    ((communicator MPI_Comm :in :MPI_COMM_WORLD)
     (errorcode :int :in -1))
    "This routine makes a 'best attempt' to abort all tasks in
the group of comm. This function does not require that the
invoking environment take any action with the error code.
However, a Unix or POSIX environment should handle this as a
return errorcode from the main program or an abort(errorcode).
See MPI_ABORT docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node151.html")

(define-api-call (%mpi-get-processor-name "MPI_Get_processor_name")
    ((processor_name :pointer)
     (namelen :pointer)))

(define-api-call (mpi-barrier "MPI_Barrier")
    ((communicator MPI_Comm :in :MPI_COMM_WORLD))
    "MPI_BARRIER blocks the caller until all group members have
called it. The call returns at any process only after all
group members have entered the call.
See MPI_BARRIER docs at:
  http://www.mpi-forum.org/docs/mpi-11-html/node66.html]")

(cffi:defcfun ("MPI_Wtime" MPI_Wtime) :double)
(cffi:defcfun ("MPI_Wtick" MPI_Wtick) :double)

;; Point-to-point communications
;; Blocking communications

(define-api-call (mpi-receive-ptr "MPI_Recv")
    ((buf :pointer)
     (count :int)
     (datatype MPI_Datatype)
     (source :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (status MPI_Status :out (:type datatype))))

(define-api-call (mpi-send-ptr/basic "MPI_Send")
    ((buf :pointer)
     (count :int)
     (datatype MPI_Datatype)
     (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-send-ptr/synchronous "MPI_Ssend")
    ((buf :pointer)
     (count :int)
     (datatype MPI_Datatype)
     (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-send-ptr/ready "MPI_Rsend")
    ((buf :pointer)
     (count :int)
     (datatype MPI_Datatype)
     (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-send-ptr/buffered "MPI_Bsend")
    ((buf :pointer)
     (count :int)
     (datatype MPI_Datatype)
     (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (%mpi-buffer-attach "MPI_Buffer_attach")
    ((buf :pointer)
     (count :int)))

(define-api-call (%mpi-buffer-detach "MPI_Buffer_detach")
    ((buf :pointer :out)
     (count :int :out)))

(define-api-call (mpi-sendrecv-ptr "MPI_Sendrecv")
    ((send-buf :pointer) (send-count :int) (send-datatype MPI_Datatype)
     (dest :int) (send-tag :int)
     (recv-buf :pointer) (recv-count :int) (recv-datatype MPI_Datatype)
     (source :int) (recv-tag :int)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (status MPI_Status :out (:type recv-datatype))))

(define-api-call (%mpi-probe "MPI_Probe")
    ((datatype MPI_Datatype :skip)
     (source :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (status MPI_Status :out (:type datatype))))

;; Non-Blocking communications (send)

(define-api-call (mpi-isend-ptr/basic "MPI_Isend")
    ((buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (request MPI_Request :out (buf count datatype))))

(define-api-call (mpi-isend-ptr/synchronous "MPI_Issend")
    ((buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (request MPI_Request :out (buf count datatype))))

(define-api-call (mpi-isend-ptr/ready "MPI_Irsend")
    ((buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (request MPI_Request :out (buf count datatype))))

(define-api-call (mpi-isend-ptr/buffered "MPI_Ibsend")
    ((buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (request MPI_Request :out (buf count datatype))))

;; Non-Blocking communications (wait)

(define-api-call (mpi-wait "MPI_Wait")
    ((request MPI_Request :mutate)
     (status MPI_Status :out (:request request)))
  "Block until the request completes.")

(define-api-call (mpi-wait-all "MPI_Waitall")
    ((seq/count :int :aux (length requests))
     (requests MPI_Request/seq :mutate)
     (statuses MPI_Status/seq :out (:request requests i)))
  "Block until all requests complete. Returns an array of matching statuses.")

(define-api-call (mpi-wait-any "MPI_Waitany")
    ((seq/count :int :aux (length requests))
     (requests MPI_Request/seq :mutate)
     (completed-index :int :out)
     (status MPI_Status :out (:request requests completed-index)))
  "Blocks until one of the requests completes. Returns (values index status)")

(define-api-call (%mpi-wait-some "MPI_Waitsome")
    ((seq/count :int :aux (length requests))
     (requests MPI_Request/seq :mutate)
     (out/count :int :out)
     (completed-indices int/seq :out)
     (statuses MPI_Status/seq :out (:request requests completed-indices i))))

(define-api-call (mpi-test "MPI_Test")
    ((request MPI_Request :mutate)
     (flag :boolean :abort-flag)
     (status MPI_Status :out (:request request)))
  "Checks whether the request completed. If yes, returns the status.")

(define-api-call (mpi-test-all "MPI_Testall")
    ((seq/count :int :aux (length requests))
     (requests MPI_Request/seq :mutate)
     (flag :boolean :abort-flag)
     (status MPI_Status/seq :out (:request requests i)))
  "Checks if all requests have completed. If yes, returns an array of statuses.")

(define-api-call (mpi-test-any "MPI_Testany")
    ((seq/count :int :aux (length requests))
     (requests MPI_Request/seq :mutate)
     (completed-index :int :out)
     (flag :boolean :abort-flag)
     (status MPI_Status :out (:request requests completed-index)))
  "Checks if any of the requests has completed. If yes, returns the index and status.")

(define-api-call (%mpi-test-some "MPI_Testsome")
    ((seq/count :int :aux (length requests))
     (requests MPI_Request/seq :mutate)
     (out/count :int :out)
     (completed-indices int/seq :out)
     (statuses MPI_Status/seq :out (:request requests completed-indices i)))
  "Checks if some of the requests have completed. Returns total count, statuses & indexes.")

;; Non-Blocking communications (probe)

(define-api-call (%mpi-iprobe "MPI_Iprobe")
    ((datatype MPI_Datatype :skip)
     (source :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (flag :boolean :abort-flag)
     (status MPI_Status :out (:type datatype))))

(define-api-call (mpi-ireceive-ptr "MPI_Irecv")
    ((buf :pointer) (count :int) (datatype MPI_Datatype) (source :int)
     (tag :int :in +default-tag+)
     (comm MPI_Comm :in :MPI_COMM_WORLD)
     (request MPI_Request :out (buf count datatype))))

;; Collective comunications

(define-api-call (mpi-broadcast-ptr "MPI_Bcast")
    ((buf :pointer) (count :int) (datatype MPI_Datatype) (root :int)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-reduce-ptr "MPI_Reduce")
    ((sendbuf :pointer) (recvbuf :pointer)
     (count :int) (datatype MPI_Datatype)
     (op MPI_Op) (root :int)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-all-reduce-ptr "MPI_Allreduce")
    ((sendbuf :pointer) (recvbuf :pointer)
     (count :int) (datatype MPI_Datatype)
     (op MPI_Op)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-scatter-ptr "MPI_Scatter")
    ((sendbuf :pointer) (sendcount :int) (sendtype MPI_Datatype)
     (recvbuf :pointer) (recvcount :int) (recvtype MPI_Datatype)
     (root :int)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-gather-ptr "MPI_Gather")
    ((sendbuf :pointer) (sendcount :int) (sendtype MPI_Datatype)
     (recvbuf :pointer) (recvcount :int) (recvtype MPI_Datatype)
     (root :int)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

(define-api-call (mpi-all-gather-ptr "MPI_Allgather")
    ((sendbuf :pointer) (sendcount :int) (sendtype MPI_Datatype)
     (recvbuf :pointer) (recvcount :int) (recvtype MPI_Datatype)
     (comm MPI_Comm :in :MPI_COMM_WORLD)))

