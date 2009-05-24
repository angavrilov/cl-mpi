#################################################
#
# Some demonstration programs for CL-MPI
#

SBCL = /usr/local/bin/sbcl
CMUCL = /usr/local/bin/lisp

# number of processes to use (you should use at least 2, as some of the demos require 2 processors)

NUM_PROCS = 4

sbcl-hello-world:
	$(SBCL) --eval "(progn (compile-file \"mpi-hello-world.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "mpi-hello-world"

cmucl-hello-world:
	mpirun -np $(NUM_PROCS) $(CMUCL) -load "mpi-hello-world"



sbcl-integrate:
	$(SBCL) --eval "(progn (compile-file \"trapezoidal-integration.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "trapezoidal-integration"
cmucl-integrate:
	mpirun -np $(NUM_PROCS) /usr/local/bin/sbcl --load "trapezoidal-integration"




sbcl-mandelbrot:
	$(SBCL) --eval "(progn (compile-file \"mandelbrot.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "mandelbrot" --eval "(progn (mandel 1000 1024 1024) (sb-ext:quit))"



sbcl-evo-compress:
	$(SBCL) --eval "(progn (compile-file \"evo-lisa.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "evo-lisa" --eval "(progn (evo-lisa:evo-lisa \"ml.gif\" 30 1000)(sb-ext:quit))"



clean:
	rm -f *~ *.o *.fasl  *.x86f *.sse2f


