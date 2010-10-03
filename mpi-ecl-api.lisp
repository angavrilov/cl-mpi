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

(defun %emit-mpi-call (stream lisp-name c-name args)
  (format stream "  ErrCode = ~A(~{~A~^,~});~%" c-name args)
  (format stream "  if (ErrCode != 0)
    cl_error(5,@mpi-error,@:failed-function,@~A,@:error-code,ecl_make_int(ErrCode));~%" lisp-name))

(defun %build-api-invoke-body (lisp-name c-name parameters)
  (let ((c-parameter-alist nil)         ; (form . type) -> refname
        (results nil)                   ; list of (type . name)
        (temporaries nil)               ; list of (type name init)
        (parameter-temporaries nil)     ; param -> tempname
        (inits nil)                     ; init statement strings
        (call-args nil)                 ; argument strings
        (aborts nil)                    ; list of (return-str . tmp-str)
        (abort-results nil)             ; list of (ret-name . ret-value)
        (mutates nil)                   ; mutate statement strings
        (returns nil))                  ; return statement strings
    (labels ((make-temporary (type &optional init &key name)
               (assert (stringp type))
               (let* ((id (length temporaries))
                      (name (or name (format nil "tmp~A" id))))
                 (push (list type name init) temporaries)
                 name))
             (register-result (type)
               (let ((idx (length results))
                     (name (gensym)))
                 (push (cons type name) results)
                 (values (format nil "@(return ~A)" idx) name)))
             ;; Marshals a parameter into the c-inline form
             (get-c-parameter (form ffi-type &aux (key (cons form ffi-type)))
               (or (cdr (assoc key c-parameter-alist :test #'equal))
                   (let* ((id (length c-parameter-alist))
                          (name (format nil "(#~36R)" id)))
                     ;; Store integer parameters in a temporary
                     ;; in order to avoid casting multiple times
                     (when (member ffi-type '(:int :boolean))
                       (setf name (make-temporary "int" name)))
                     (push (cons key name) c-parameter-alist)
                     name)))
             ;; FFI type conversion
             (cffi-to-ecl-type (type)
               (case type
                 (:pointer :pointer-void)
                 (:boolean :bool)
                 (t type)))
             (ffi-to-c-type (type)
               (or (case type
                     (:pointer "void*")
                     ((:int :boolean int/seq) "int")
                     ((MPI_Request MPI_Request/seq) "MPI_Request")
                     ((MPI_Status MPI_Status/seq) "MPI_Status"))
                   (get type 'parsed-c-type)
                   (error "Unknown ffi type: ~S" type)))
             (outer-ffi-type (type)
               (or (case type
                     ((MPI_Request MPI_Request/seq MPI_Status MPI_Status/seq int/seq)
                      :object))
                   (get type 'input-ffi-type)
                   type))
             ;; Allocate a temporary for a parameter
             (make-parameter-temporary (type seq? param)
               (let* ((tname (ffi-to-c-type type))
                      (vtype (if seq? (format nil "~A*" tname) tname))
                      (init (if seq?
                                (format nil "ecl_alloc_atomic(sizeof(~A)*~A)"
                                        tname (get-c-parameter 'seq/count :int))))
                      (tmp-name (make-temporary vtype init)))
                 (push (cons param tmp-name) parameter-temporaries)
                 tmp-name))
             ;; Init a var or a sequence
             (init-temporary (tmp-name seq? in-arg initfun)
               (if seq?
                   (let ((count (get-c-parameter 'seq/count :int)))
                     (format nil "for (i = 0; i < ~A; i++) {~%  ~A~%  }"
                             count
                             (funcall initfun
                                      (format nil "~A[i]" tmp-name)
                                      (format nil "ecl_elt(~A,i)" in-arg))))
                   (funcall initfun tmp-name in-arg)))
             (init-var-temporary (type seq? param tmp-name)
               (let ((in-arg (get-c-parameter param (outer-ffi-type type)))
                     (initfun (or (get type 'emit-parser)
                                  (lambda (out in)
                                    (case type
                                      ((MPI_Request MPI_Request/seq)
                                       (%emit/mpi-request->MPI_Request out in))
                                      (t
                                       (format nil "~A = ~A;" out in)))))))
                 (push (init-temporary tmp-name seq? in-arg initfun) inits)))
             ;; Return a var or a sequence
             (return-temporary (ret-expr seq? tmp-name export-fun)
               (if seq?
                   (let* ((out/count (cdr (assoc 'out/count parameter-temporaries)))
                          (count (or out/count (get-c-parameter 'seq/count :int)))
                          (arr (make-temporary "cl_object"))
                          (exp-loop
                           (format nil "for (i = 0; i < ~A; i++) {~%  ~A~%  }"
                                   count
                                   (funcall export-fun
                                            (format nil "~A->array.self.t[i]" arr)
                                            (format nil "~A[i]" tmp-name))))
                          (build-code
                           (format nil "~A = cl_make_array(1,ecl_make_int(~A));~%  ~A~%  ~A = ~A;"
                                   arr count exp-loop ret-expr arr)))
                     (if out/count
                         (format nil "if (~A != MPI_UNDEFINED) {  ~A~%  } else ~A = Cnil;"
                                 count build-code arr ret-expr)
                         build-code))
                   (funcall export-fun ret-expr tmp-name)))
             (return-var-temporary (type seq? param tmp-name default)
               (let ((ret-expr (register-result (outer-ffi-type type)))
                     (export-fun (lambda (out in)
                                   (case type
                                     ((MPI_Request MPI_Request/seq)
                                      (apply #'%emit/MPI_Request->mpi-request
                                             out in #'get-c-parameter default))
                                     ((MPI_Status MPI_Status/seq)
                                      (apply #'%emit/MPI_Status->mpi-status
                                             out in #'get-c-parameter
                                             parameter-temporaries default))
                                     ((int/seq)
                                      (format nil "~A = ecl_make_int(~A);" out in))
                                     (t
                                      (format nil "~A = ~A;" out in))))))
                 (push (return-temporary ret-expr seq? tmp-name export-fun) returns))))
      ;; Scan parameters
      (loop for (param type mode default) in parameters
         for in? = (member mode '(nil :in :inout :skip :mutate))
         for out? = (member mode '(:out :inout :mutate :abort-flag))
         for mutate? = (eq mode :mutate)
         for seq? = (member type '(MPI_Request/seq MPI_Status/seq int/seq))
         do (if (not (or out? seq? (get type 'emit-parser)))
                (push (get-c-parameter param type) call-args)
                (let ((tmp-name (make-parameter-temporary type seq? param))
                      (outer-type (outer-ffi-type type)))
                  (when in?
                    (init-var-temporary type seq? param tmp-name))
                  (unless (eq mode :skip)
                    (push (if (or seq? (not out?)) tmp-name
                              (format nil "&~A" tmp-name)) call-args))
                  (cond (mutate?
                         (ecase type
                           ((MPI_Request MPI_Request/seq)
                            (push (init-temporary tmp-name seq?
                                                  (get-c-parameter param outer-type)
                                                  #'%emit/MPI_Request->update)
                                  mutates))))
                        ((eq mode :abort-flag)
                         (multiple-value-bind (rv name)
                             (register-result :bool)
                           (push (cons rv tmp-name) aborts)
                           (push (cons name default) abort-results)))
                        (out?
                         (return-var-temporary type seq? param tmp-name default))))))
      ;; Assemble the form
      (let* ((params (nreverse (mapcar #'car c-parameter-alist)))
             (results (nreverse results))
             (body (with-output-to-string (stream)
                     (format stream "{ int ErrCode, i;~%")
                     (loop for (type name init) in (reverse temporaries)
                        do (format stream "  ~A ~A~@[ = ~A~];~%" type name init))
                     (dolist (str (reverse inits))
                       (format stream "  ~A~%" str))
                     (%emit-mpi-call stream lisp-name c-name (reverse call-args))
                     (loop for (rv . tmp) in aborts
                        do (format stream "  ~A = ~A;~%" rv tmp))
                     (when aborts
                       (format stream "  if (~{~A~^ && ~}) {~%" (mapcar #'cdr aborts)))
                     (dolist (str (reverse mutates))
                       (format stream "  ~A~%" str))
                     (dolist (str (reverse returns))
                       (format stream "  ~A~%" str))
                     (when aborts
                       (format stream "}"))
                     (format stream "}")))
             (inline-form
              `(ffi:c-inline ,(mapcar #'car params)
                             ,(mapcar #'cffi-to-ecl-type (mapcar #'cdr params))
                   (values ,@(mapcar #'cffi-to-ecl-type (mapcar #'car results)))
                 ,body)))
        (if abort-results
            `(multiple-value-bind ,(mapcar #'cdr results)
                 ,inline-form
               (cond ,@(loop for (name . default) in (reverse abort-results)
                          collect `((not ,name) ,default))
                     (t (values ,@(remove-if (lambda (x) (assoc x abort-results))
                                             (mapcar #'cdr results))))))
            inline-form)))))

(defmacro define-api-call ((lisp-name c-name &key disabled) parameters &optional documentation)
  "Builds a trivial C function call wrapper."
  (multiple-value-bind (fun-args)
      (loop with in-optional? = nil
         for (param type mode default) in parameters
         for in? = (member mode '(nil :in :inout :skip :mutate))
         for aux? = (eq mode :aux)
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
         ;; Return
         finally (return (nconc fun-args (if aux-args (list* '&aux aux-args)))))
    (let* ((invoke-body (%build-api-invoke-body lisp-name c-name parameters))
           (body (if (eq disabled :call)
                     invoke-body
                     `(if *enable-mpi* ,invoke-body ,disabled))))
      `(defun ,lisp-name ,fun-args
         ,documentation
         ,body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface between foreign and lisp objects
;;;

;; Status

(defparameter %mpi-status-template% (make-status))

(defun %emit/MPI_Status->mpi-status (out status-ref arg-cb temp-alist mode rq-or-type &rest indexes)
  (with-output-to-string (str)
    (format str "{ cl_object type_tmp, rq_tmp, tmp = Cnil;")
    (format str "    int type, idx, count = -1;~%")
    (ecase mode
      (:type
       (let ((name (cdr (assoc rq-or-type temp-alist))))
         (assert (stringp name))
         (format str "    type = ~A;~%" name)))
      (:request
       (let* ((rindexes (reverse indexes)))
         (format str "    rq_tmp = ~A;~%" (funcall arg-cb rq-or-type :object))
         (when indexes
           (if (eq (first rindexes) 'i)
               (format str "    idx = i;~%")
               (format str "    if ((idx = ~A) == MPI_UNDEFINED) goto skip_status;~%"
                       (cdr (assoc (first rindexes) temp-alist))))
           (loop for rtable-name in (rest rindexes)
              do (format str "    idx = ~A[idx];~%"
                         (cdr (assoc rtable-name temp-alist))))
           (format str "    rq_tmp = ecl_elt(rq_tmp,idx);~%"))
         (format str "    type_tmp = SLOT(rq_tmp,3);~%")
         (format str "    if (type_tmp == Cnil) goto skip_type;~%")
         (format str "  ~A~%" (funcall (get 'MPI_Datatype 'emit-parser) "type" "type_tmp")))))
    (%emit-mpi-call str '%mpi-get-count "MPI_Get_count"
                    (list (format nil "&~A" status-ref) "type" "&count"))
    (format str "skip_type:~%")
    (format str "    tmp = cl_copy_structure((@%mpi-status-template%)->symbol.value);~%")
    (format str "    SLOT(tmp,0) = (count < 0) ? Cnil : ecl_make_int(count);~%")
    (format str "    SLOT(tmp,1) = ecl_make_int(~A.MPI_SOURCE);~%" status-ref)
    (format str "    SLOT(tmp,2) = ecl_make_int(~A.MPI_TAG);~%" status-ref)
    (format str "    SLOT(tmp,3) = ecl_make_int(~A.MPI_ERROR);~%" status-ref)
    (when (eq mode :request)
      (format str "    if (SLOT(rq_tmp,4) != Cnil) cl_funcall(3,SLOT(rq_tmp,4),rq_tmp,tmp);~%"))
    (format str "skip_status:~%    ~A = tmp;~%" out)
    (format str "  }")))

;; Request

(defparameter %mpi-request-template% (make-request))

(defun %emit/mpi-request->MPI_Request (output input)
  (format nil "~A = (MPI_Request)ecl_to_pointer(ecl_structure_ref(~A,@mpi::request,0));"
          output input))

(defun %emit/MPI_Request->update (tmp-var struct)
  ;; Don't check types since they must have already been
  ;; checked by the previous function.
  (with-output-to-string (str)
    (format str "{ cl_object rq_struct = ~A; cl_object ptr = SLOT(rq_struct,0);~%" struct)
    (format str "    MPI_Request new_value = ~A, old_value = (MPI_Request)(ptr->foreign.data);~%" tmp-var)
    (format str "    if (new_value != old_value) {~%")
    (format str "      if (new_value != MPI_REQUEST_NULL) FEerror(\"Request updated to not null\",0);~%")
    (format str "      ptr->foreign.data = (void*)new_value;~%")
    (format str "    }~%  }")))

(defun %emit/MPI_Request->mpi-request (out rq-ptr arg-cb &optional buffer count datatype)
  ;; This depends on hard-coded positions of the first 4 slots of
  ;; the request structure. In order to handle the rest of the
  ;; structure safely, allocation is performed by copying a template.
  (with-output-to-string (str)
    (format str "{ cl_object tmp = cl_copy_structure((@%mpi-request-template%)->symbol.value);~%")
    (format str "    SLOT(tmp,0) = ecl_make_pointer((void*)~A);~%" rq-ptr)
    (when buffer
      (format str "    SLOT(tmp,1) = ~A;~%" (funcall arg-cb buffer :object)))
    (when count
      (format str "    SLOT(tmp,2) = ~A;~%" (funcall arg-cb count :object)))
    (when datatype
      (format str "    SLOT(tmp,3) = ~A;~%" (funcall arg-cb datatype :object)))
    (format str "    ~A = tmp;~%" out)
    (format str "  }")))

