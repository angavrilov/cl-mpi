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

(load-mpi-foreign-libraries)

;; Environmental functions
(cffi:defcfun ("MPI_Initialized" MPI_Initialized) :int (flag :pointer))
(cffi:defcfun ("MPI_Init" MPI_Init) :int (argc :pointer) (argv :pointer))
(cffi:defcfun ("MPI_Finalize" MPI_Finalize) :int )

(cffi:defcfun ("MPI_Comm_rank" MPI_Comm_rank) :int (communicator MPI_Comm)(myid :pointer))
(cffi:defcfun ("MPI_Comm_size" MPI_Comm_size) :int (communicator MPI_Comm)(numprocs :pointer))
(cffi:defcfun ("MPI_Get_processor_name" MPI_Get_processor_name) :int (processor_name :string) (namelen :pointer))
(cffi:defcfun ("MPI_Barrier" MPI_Barrier) :int (communicator MPI_Comm))
(cffi:defcfun ("MPI_Wtime" MPI_Wtime) :double )
(cffi:defcfun ("MPI_Wtick" MPI_Wtick) :double )
(cffi:defcfun ("MPI_Abort" MPI_Abort) :int (comm MPI_Comm) (errorcode :int))

;; Point-to-point communications

(cffi:defcfun ("MPI_Get_count" MPI_Get_count) :int (status :pointer) (datatype MPI_Datatype) (count :pointer))

;;       Blocking communications
(cffi:defcfun ("MPI_Recv" MPI_Recv):int (buf :pointer)(count :int) (datatype MPI_Datatype)(source :int)(tag :int)(comm MPI_Comm)(status :pointer))
(cffi:defcfun ("MPI_Send" MPI_Send) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Ssend" MPI_Ssend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Rsend" MPI_Rsend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Bsend" MPI_Bsend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Buffer_attach" MPI_Buffer_attach) :int (buf :pointer) (count :int));;int MPI_Buffer_attach( void*, int);
(cffi:defcfun ("MPI_Buffer_detach" MPI_Buffer_detach) :int (buf :pointer) (count-ptr :pointer));;int MPI_Buffer_detach( void*, int*);

(cffi:defcfun ("MPI_Sendrecv" MPI_Sendrecv) :int 
  (send-buf :pointer)(send-count :int) (send-datatype MPI_Datatype)(dest :int) (send-tag :int)
  (recv-buf :pointer)(recv-count :int) (recv-datatype MPI_Datatype)(source :int) (recv-tag :int)
  (comm MPI_Comm)(status :pointer))

(cffi:defcfun ("MPI_Probe" MPI_Probe) :int (source :int)(tag :int)(communicator MPI_Comm)  (status :pointer))


;;       Non-Blocking communications
(cffi:defcfun ("MPI_Isend" MPI_Isend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(cffi:defcfun ("MPI_Ibsend" MPI_Ibsend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(cffi:defcfun ("MPI_Issend" MPI_Issend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(cffi:defcfun ("MPI_Irsend" MPI_Irsend) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))

(cffi:defcfun ("MPI_Wait" MPI_Wait) :int (request :pointer) (status :pointer))
(cffi:defcfun ("MPI_Waitall" MPI_Waitall) :int (count :int)(requests :pointer) (statuses :pointer))
(cffi:defcfun ("MPI_Waitany" MPI_Waitany) :int (count :int)(requests :pointer)(completed-index :pointer)(status :pointer))
(cffi:defcfun ("MPI_Waitsome" MPI_Waitsome) :int (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

(cffi:defcfun ("MPI_Test" MPI_Test) :int (request :pointer) (flag :pointer) (status :pointer))
(cffi:defcfun ("MPI_Testall" MPI_Testall) :int (count :int) (requests :pointer) (flag :pointer) (statuses :pointer))
(cffi:defcfun ("MPI_Testany" MPI_Testany) :int (count :int) (requests :pointer) (index :pointer)(flag :pointer) (statuses :pointer))
(cffi:defcfun ("MPI_Testsome" MPI_Testsome) :int (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))
;; int MPI_Request_free(MPI_Request *);

(cffi:defcfun ("MPI_Iprobe" MPI_Iprobe) :int (source :int)(tag :int)(communicator MPI_Comm)(flag :pointer)(status :pointer))

(cffi:defcfun ("MPI_Irecv" MPI_Irecv) :int (buf :pointer)(count :int)(datatype MPI_Datatype)(source :int)(tag :int)(comm MPI_Comm)(request :pointer))
;; MPI_Request* is a pointer to a really hairy union MPIR_HANDLE, which is defined in, e.g., /mpich-1.2.7p1/mpid/ch2/req.h
;;    *** I don't want to directly access anything inside a MPI_Request!!

;; Collective comunications
(cffi:defcfun ("MPI_Bcast" MPI_Bcast) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (root :int) (comm MPI_Comm))


(cffi:defcfun ("MPI_Reduce" MPI_Reduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype MPI_datatype)(op MPI_Op)(root :int)(comm MPI_Comm))
(cffi:defcfun ("MPI_Allreduce" MPI_Allreduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype MPI_datatype)(op MPI_Op)(comm MPI_Comm))
(cffi:defcfun ("MPI_Scatter" MPI_Scatter) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
	      (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(root :int)(comm MPI_Comm))
(cffi:defcfun ("MPI_Gather" MPI_Gather) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
	      (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(root :int)(comm MPI_Comm))
(cffi:defcfun ("MPI_Allgather" MPI_Allgather) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
	      (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(comm MPI_Comm))

