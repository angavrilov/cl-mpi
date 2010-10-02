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

(in-package #:mpi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trivial API wrapper definition macro
;;;

(defmacro define-api-call ((lisp-name c-name &key disabled) parameters &optional documentation)
  "Builds a trivial C function call wrapper."
  (multiple-value-bind (fun-args ctypes cargs temps inits mutates reads read-names)
      (loop with in-optional? = nil
         for (param type mode default) in parameters
         for in? = (member mode '(nil :in :inout :skip :mutate))
         for out? = (member mode '(:out :inout :mutate :abort-flag))
         for skip? = (eq mode :skip)
         for mutate? = (eq mode :mutate)
         for aux? = (eq mode :aux)
         for tsym = (gensym)
         ;; Switch to optional if default found:
         when (and in? default (not in-optional?))
         collect '&optional into fun-args
         and do (setf in-optional? t)
         ;; Function input arguments:
         when in?
         collect (if default `(,param ,default) param) into fun-args
         ;; Aux arguments
         when aux?
         collect `(,param ,default) into aux-args
         ;; Internal call type:
         unless skip?
         collect `(,param ,(if out? :pointer type)) into ctypes
         and collect (if out? tsym param) into cargs
         ;; Temporaries for output:
         when out?
         collect (case type
                   (MPI_Request/seq `(,tsym 'MPI_Request seq/count))
                   (MPI_Status/seq  `(,tsym 'MPI_Status seq/count))
                   (int/seq         `(,tsym :int seq/count))
                   (t               `(,tsym ',type)))
         into temps
         ;; Initialize temporary for inout:
         when (and in? out?)
         collect (case type
                   (MPI_Request
                    `(mpi-request->MPI_Request ,param ,tsym 0))
                   (MPI_Request/seq
                    `(dotimes (i seq/count)
                       (mpi-request->MPI_Request (elt ,param i) ,tsym i)))
                   (t
                    `(setf (cffi:mem-ref ,tsym ',type) ,param)))
         into inits
         ;; Update mutable objects
         when mutate?
         collect (ecase type
                   (MPI_Request
                    `(MPI_Request->update ,param ,tsym 0))
                   (MPI_Request/seq
                    `(dotimes (i seq/count)
                       (MPI_Request->update (elt ,param i) ,tsym i))))
         into mutates
         ;; Read temporaries for output:
         when (eq mode :abort-flag)
         collect `(unless (cffi:mem-ref ,tsym ',type)
                    (return-from ,lisp-name ,default))
         into aborts
         else when (and out? (not mutate?))
         collect
           (flet ((wrap-seq (form)
                    (if (member 'out/count parameters :key #'first)
                        `(unless (= out/count MPI_UNDEFINED)
                           (let ((,param (make-array out/count)))
                             (dotimes (i out/count ,param)
                               (setf (aref ,param i) ,form))))
                        `(let ((,param (make-array seq/count)))
                           (dotimes (i seq/count ,param)
                             (setf (aref ,param i) ,form))))))
             (case type
               (MPI_Status
                `(MPI_Status->mpi-status ,tsym ,default))
               (MPI_Status/seq
                (wrap-seq `(MPI_Status->mpi-status
                            (cffi:mem-aref ,tsym 'MPI_Status i) ,default)))
               (int/seq
                (wrap-seq `(cffi:mem-aref ,tsym :int i)))
               (MPI_Request
                `(MPI_Request->mpi-request ,tsym ,@default))
               (t
                `(cffi:mem-ref ,tsym ',type))))
         into reads
         and collect param into read-names
         ;; Return
         finally (return (values (nconc fun-args (if aux-args (list* '&aux aux-args)))
                                 ctypes cargs temps inits
                                 (nconc aborts mutates)
                                 reads read-names)))
    (let* ((cfun-name (intern (format nil "%~A/RAW" lisp-name)))
           (invoke-body
            `(cffi:with-foreign-objects ,temps
               ,@inits
               (let ((err (,cfun-name ,@cargs)))
                 (when (/= err 0)
                   (error 'mpi-error :failed-function ',lisp-name :error-code err)))
               ,@mutates
               (let* ,(mapcar #'list read-names reads)
                 (values ,@read-names))))
           (body (if (eq disabled :call)
                     invoke-body
                     `(if *enable-mpi* ,invoke-body ,disabled))))
      `(progn
         (cffi:defcfun (,c-name ,cfun-name) :int
           ,@ctypes)
         (defun ,lisp-name ,fun-args
           ,documentation
           ,body)))))

(defmacro %index-request-datatype (requests index &optional index2)
  (let ((iexpr (if index2 `(elt ,index ,index2) index)))
    (if (eq (or index2 index) 'i)
        `(request-datatype (elt ,requests ,iexpr))
        `(unless (= ,index MPI_UNDEFINED)
           (request-datatype (elt ,requests ,iexpr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface between foreign and lisp objects
;;;

;; Status

(define-api-call (%mpi-get-count "MPI_Get_count" :disabled :call)
    ((status :pointer)
     (datatype MPI_Datatype)
     (count :int :out)))

(declaim (inline MPI_Status->mpi-status))

(defun MPI_Status->mpi-status (status mpi-type)
  "Returns a Lisp-space status object, equivalent to the MPI_Status object."
  (make-status :count (if mpi-type
                          (%mpi-get-count status mpi-type)
                          nil)
               :source (cffi:foreign-slot-value status 'MPI_Status 'MPI_SOURCE)
               :tag (cffi:foreign-slot-value status 'MPI_Status 'MPI_TAG)
               :error (cffi:foreign-slot-value status 'MPI_Status 'MPI_ERROR)))

;; Request

(declaim (inline MPI_Request->mpi-request mpi-request->MPI_Request))

(defun MPI_Request->mpi-request (rq-ptr &optional buffer (count 0) datatype)
  "Returns a Lisp-space request object, wrapping MPI_Request"
  (make-request :mpi-request (cons (cffi:mem-ref rq-ptr 'MPI_Request) t)
                :buf buffer :count count :datatype datatype))

(defun mpi-request->MPI_Request (request rq-ptr index)
  (setf (cffi:mem-aref rq-ptr 'MPI_Request index)
        (car (request-mpi-request request))))

(defvar *null-request-handle* nil)

(defun request-handle= (new-value old-value)
  (or (eql new-value old-value)
      #+sbcl
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note warning))
        (and (sb-sys:system-area-pointer-p old-value)
             (sb-sys:sap= old-value new-value)))))

(defun request-deallocated-p (request)
  (null (cdr (request-mpi-request request))))

(defun MPI_Request->update (request rq-ptr index)
  (let* ((new-value (cffi:mem-aref rq-ptr 'MPI_Request index))
         (handle (request-mpi-request request))
         (old-value (car handle)))
    (unless (request-handle= new-value old-value)
      (if (null *null-request-handle*)
          (setf *null-request-handle* new-value)
          (assert (request-handle= new-value *null-request-handle*)))
      (setf (car handle) new-value
            (cdr handle) nil))))

