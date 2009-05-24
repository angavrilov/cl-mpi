#################################################
#
#
# look for MPI installation; generate specialized version of grovel file

# export CC = mpicc so that cffi-grovel can compile mpi-grovel correctly

SBCL = /usr/local/bin/sbcl
CMUCL = /usr/local/bin/lisp

# number of procs to 
NUMPROCS = 2

cl-mpi-sbcl:
	export CC=mpicc
	$(SBCL) --load "make-mpi.lisp"

test-sbcl: cl-mpi-sbcl
	mpirun -np $(NUMPROCS) $(SBCL) --load "run-mpi-test.lisp"


cl-mpi-cmucl:
	$(CMUCL) -load "make-mpi.lisp"

test-cmucl: cl-mpi-cmucl
	mpirun -np $(NUMPROCS) $(CMUCL) -load "run-mpi-test.lisp"

####################################################
# In order to make CL-MPI usable with ASDF
# The following  creates links from ~/.sbcl/site and ~/.sbcl/systems to this directory

CURRENT_DIR = $(shell pwd)
ASDF_SITE_DIR = $(HOME)/.sbcl/site
ASDF_SYSTEMS_DIR = $(HOME)/.sbcl/systems/

asdf-links:
	ln -s -i $(CURRENT_DIR) $(ASDF_SITE_DIR)/cl-mpi
	ln -s -i $(CURRENT_DIR)/cl-mpi.asd $(ASDF_SYSTEMS_DIR)/
	ln -s -i $(CURRENT_DIR)/par-eval.asd $(ASDF_SYSTEMS_DIR)/

remove-asdf-links:
	rm -f $(ASDF_SYSTEMS_DIR)/cl-mpi.asd
	rm -f $(ASDF_SYSTEMS_DIR)/par-eval.asd
	rm -f $(ASDF_SITE_DIR)/cl-mpi

####################################################

clean:
	rm -f *.o *.fasl *.x86f *.sse2f mpi-grovel.c mpi-grovel.grovel-tmp.lisp mpi-grovel

