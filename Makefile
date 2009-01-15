#################################################
#
#
# look for MPI installation; generate specialized version of grovel file


MPI_HEADER_PATH = /usr/include/mpi/mpi.h

cl-mpi:
	sbcl --eval "(progn (asdf:operate 'asdf:load-op 'cl-mpi)(sb-ext:quit))"

test: cl-mpi
	mpirun -np 2 /usr/local/bin/sbcl --load "run-mpi-test.lisp"

clean:
	rm -f *.o *.fasl mpi-grovel.c mpi-grovel.grovel-tmp.lisp mpi-grovel
