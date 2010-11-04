(include #.mpi::*mpi-header-file*)

(in-package :MPI)

;;/* Communicators */
(constantenum MPI_Comm
	      ((:MPI_COMM_WORLD "MPI_COMM_WORLD"))
	      ((:MPI_COMM_SELF "MPI_COMM_SELF")))

(constantenum MPI_Datatype
	      ((:MPI_CHAR "MPI_CHAR"))
	      ((:MPI_BYTE "MPI_BYTE"))
	      ((:MPI_UNSIGNED_CHAR "MPI_UNSIGNED_CHAR"))
	      ((:MPI_SHORT "MPI_SHORT"))
	      ((:MPI_UNSIGNED_SHORT "MPI_UNSIGNED_SHORT"))
	      ((:MPI_INT "MPI_INT"))
	      ((:MPI_UNSIGNED "MPI_UNSIGNED"))
	      ;;((:MPI_LONG "MPI_LONG"))
	      ;;((:MPI_UNSIGNED_LONG "MPI_UNSIGNED_LONG"))
	      ((:MPI_LONG_LONG
                "MPI_LONG_LONG" "MPI_LONG_LONG_INT"))
	      ((:MPI_UNSIGNED_LONG_LONG
                "MPI_UNSIGNED_LONG_LONG" "MPI_LONG_LONG" "MPI_LONG_LONG_INT"))
	      ((:MPI_FLOAT "MPI_FLOAT"))
	      ((:MPI_DOUBLE "MPI_DOUBLE")))

(constantenum MPI_Op
	      ((:MPI_MAX "MPI_MAX"))
	      ((:MPI_MIN "MPI_MIN"))
	      ((:MPI_SUM "MPI_SUM"))
	      ((:MPI_PROD "MPI_PROD"))
	      ((:MPI_LAND "MPI_LAND"))
	      ((:MPI_BAND "MPI_BAND"))
	      ((:MPI_LOR  "MPI_LOR"))
	      ((:MPI_BOR  "MPI_BOR"))
	      ((:MPI_LXOR "MPI_LXOR"))
	      ((:MPI_BXOR "MPI_BXOR"))
	      ((:MPI_MINLOC "MPI_MINLOC"))
	      ((:MPI_MAXLOC "MPI_MAXLOC")))

(constant (MPI_SUCCESS "MPI_SUCCESS"))
(constant (MPI_UNDEFINED "MPI_UNDEFINED"))
(constant (MPI_ANY_TAG "MPI_ANY_TAG"))
(constant (MPI_ANY_SOURCE "MPI_ANY_SOURCE"))

(constant (MPI_MAX_PROCESSOR_NAME "MPI_MAX_PROCESSOR_NAME"))

;; Request type

(ctype MPI_Request "MPI_Request")

;; The cancelled field is new in MPI2, not in MPI1.
;; It seems OK to just ignore this field, since I'm not using it yet in CL-MPI.
(cstruct MPI_Status "MPI_Status"
	 (count "count" :type :int)
	 ;;(cancelled "cancelled" :type :int) ;;<-- MPICH1/2 incompatibility ignore this field for now.
	 ;;(cancelled "_cancelled" :type :int) ;;<-- OpenMPI
	 (MPI_SOURCE "MPI_SOURCE" :type :int)
	 (MPI_TAG "MPI_TAG" :type :int)
	 (MPI_ERROR "MPI_ERROR" :type :int))

