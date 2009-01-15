#################################################
#
#
# look for MPI installation; generate specialized version of grovel file


SBCL = /usr/local/bin/sbcl

cl-mpi:
	$(SBCL) --load "make-mpi.lisp"

test: cl-mpi
	mpirun -np 2 $(SBCL) --load "run-mpi-test.lisp"

clean:
	rm -f *.o *.fasl mpi-grovel.c mpi-grovel.grovel-tmp.lisp mpi-grovel
