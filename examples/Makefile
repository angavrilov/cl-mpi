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
	mpirun -np $(NUM_PROCS) $(SBCL) --load "mpi-hello-world" --eval "(progn (mpi-hello-world)(sb-ext:quit))"

cmucl-hello-world:
	$(CMUCL) -eval "(progn (compile-file \"mpi-hello-world.lisp\")(ext:quit))"
	mpirun -np $(NUM_PROCS) $(CMUCL) -load "mpi-hello-world" -eval "(progn (mpi-hello-world)(ext:quit))"



sbcl-integrate:
	$(SBCL) --eval "(progn (compile-file \"trapezoidal-integration.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "trapezoidal-integration"
cmucl-integrate:
	mpirun -np $(NUM_PROCS) /usr/local/bin/sbcl --load "trapezoidal-integration"


sbcl-mandel-bench1:
	$(SBCL) --eval "(progn (compile-file \"mandel-bench-1.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "mandel-bench-1" --eval "(progn (main) (sb-ext:quit))"


sbcl-mandelbrot:
	$(SBCL) --eval "(progn (compile-file \"mandelbrot.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "mandelbrot" --eval "(progn (mandel 100 5000 5000) (sb-ext:quit))"



sbcl-evo-compress:
	$(SBCL) --eval "(progn (compile-file \"evo-lisa.lisp\")(sb-ext:quit))"
	mpirun -np $(NUM_PROCS) $(SBCL) --load "evo-lisa" --eval "(progn (evo-lisa:evo-lisa \"ml.gif\" 30 1000)(sb-ext:quit))"



clean:
	rm -f *~ *.o *.fasl  *.x86f *.sse2f


