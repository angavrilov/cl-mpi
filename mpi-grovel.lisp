;;(include "/usr/include/mpi/mpi.h")
(include #.mpi::*mpi-header-file*)

(in-package :MPI)

;;/* Communicators */
;;typedef int MPI_Comm;
(constantenum MPI_Comm
	      ((:MPI_COMM_WORLD "MPI_COMM_WORLD"))
	      ((:MPI_COMM_SELF "MPI_COMM_SELF")))

(constantenum MPI_Datatype
	      ((:MPI_CHAR "MPI_CHAR"))
	      ((:MPI_UNSIGNED_CHAR "MPI_UNSIGNED_CHAR"))
	      ((:MPI_SHORT "MPI_SHORT"))
	      ((:MPI_UNSIGNED_SHORT "MPI_UNSIGNED_SHORT"))
	      ((:MPI_INT "MPI_INT"))
	      ((:MPI_UNSIGNED "MPI_UNSIGNED"))
	      ((:MPI_LONG "MPI_LONG"))
	      ((:MPI_UNSIGNED_LONG "MPI_UNSIGNED_LONG"))
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
