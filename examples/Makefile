#################################################
#
#
# look for MPI installation; generate specialized version of grovel file


SBCL = /usr/local/bin/sbcl
CMUCL = /usr/local/bin/lisp

sbcl-test-hello-world:
	mpirun -np 3 /usr/local/bin/sbcl --load "mpi-hello-world"
cmucl-test-hello-world:
	mpirun -np 3 /usr/local/bin/cmucl -load "mpi-hello-world"

sbcl-test-integrate:
	mpirun -np 3 /usr/local/bin/sbcl --load "trapezoidal-integration"
cmucl-test-integrate:
	mpirun -np 3 /usr/local/bin/sbcl --load "trapezoidal-integration"

clean:
	rm -f *~ *.o *.fasl 

