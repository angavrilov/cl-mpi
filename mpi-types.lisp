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
|#

(in-package #:MPI)

;;; Error condition

(define-condition mpi-error (simple-error)
  ((failed-function :initform nil :initarg :failed-function
                    :reader failed-function-of)
   (error-code :initform nil :initarg :error-code
               :reader error-code-of))
  (:default-initargs :format-control "An MPI API call has failed." :format-arguments nil))

(defun mpi-error (message &rest args)
  (error 'mpi-error :format-control message :format-arguments args))

(defmethod print-object :after ((obj mpi-error) stream)
  (unless (or *print-escape*
              (and (null (failed-function-of obj))
                   (null (error-code-of obj))))
    (format stream "~&Failed function: ~A, error code: ~A"
            (failed-function-of obj) (error-code-of obj))))

;;; Wrappers for low-level structures

(defstruct status
  "Represents the completion status of an MPI operation."
  ;; will usually be a (signed-byte 32), but nil means that the count
  ;; could not be computed with MPI_Get_count at the time this status
  ;; object was created
  (count nil)
  (source 0 :type (signed-byte 32))
  (tag 0 :type (signed-byte 32))
  (error 0 :type (signed-byte 32))
  #| NOTE: The slots before this place are accessed by
  .        ECL wrappers using hard-coded positions. |#)

(defstruct typespec
  "Represents a lisp type known to the MPI library."
  (lisp-type nil :read-only t)
  (size nil :read-only t)
  (mpi-type nil :read-only t)
  (cffi-type nil :read-only t)
  (id nil :read-only t)
  (unsafe-p nil :read-only t))

(defstruct obj-tspec
  "Metadata for an object to be transferred."
  ;; ID code
  (id (error "ID required"))
  ;; lisp type
  (type)
  ;; # of base-type objects in this object
  (count)
  ;; the base typespec for this object
  (base-typespec)
  ;; if converted, hold the converted representation of the object here
  (converted-obj nil))

(defstruct request
  "An object representing a nonblocking receive."
  ;; the MPI request handle (opaque)
  (mpi-request nil)
  ;; buffer which will be filled when the request is complete
  (buf nil)
  ;; # of received objects, from the status
  (count 0 :type fixnum)
  ;; data type
  (datatype nil)
  #| NOTE: The slots before this place are accessed by
  .        ECL wrappers using hard-coded positions. |#)

;;; Some constants and vars referred to by mpi-bindings.lisp

(defconstant +default-tag+ 1 "default tag for MPI_Send and MPI_Recv")

(defparameter *enable-mpi* t)

