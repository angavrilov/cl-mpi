#################################################
#
#
# look for MPI installation; generate specialized version of grovel file


SBCL = /usr/local/bin/sbcl
CMUCL = /usr/local/bin/lisp

cl-mpi-sbcl:
	$(SBCL) --load "make-mpi.lisp"

test-sbcl: cl-mpi-sbcl
	mpirun -np 2 $(SBCL) --load "run-mpi-test.lisp"


cl-mpi-cmucl:
	$(CMUCL) -load "make-mpi.lisp"

test-cmucl: cl-mpi-cmucl
	mpirun -np 2 $(CMUCL) -load "run-mpi-test.lisp"


CURRENT_DIR = $(shell pwd)
# This will create links from ~/.sbcl/site and ~/.sbcl/systems
install-hack:
	ln -s $(CURRENT_DIR) $(HOME)/.sbcl/site/cl-mpi
	ln -s  $(CURRENT_DIR)/cl-mpi.asd $(HOME)/.sbcl/systems/
remove-links:
	rm -f $(HOME)/.sbcl/systems/cl-mpi.asd
	rm -f $(HOME)/.sbcl/site/cl-mpi

clean:
	rm -f *.o *.fasl mpi-grovel.c mpi-grovel.grovel-tmp.lisp mpi-grovel

