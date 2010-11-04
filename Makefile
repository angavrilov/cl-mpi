#################################################
#
#
# look for MPI installation; generate specialized version of grovel file

# export CC = mpicc so that cffi-grovel can compile mpi-grovel correctly

SBCL = /usr/local/bin/sbcl
CMUCL = /usr/local/bin/lisp
ECL = ecl

# number of procs to 
NUMPROCS = 2

cl-mpi-sbcl:
	export CC=mpicc
	$(SBCL) --load "make-mpi.lisp"

test-sbcl: cl-mpi-sbcl
	mpirun -np $(NUMPROCS) $(SBCL) --load "run-mpi-test.lisp"

cl-mpi-ecl:
	$(ECL) -load "make-mpi.lisp"

test-ecl: cl-mpi-ecl
	mpirun -np $(NUMPROCS) $(ECL) -load "run-mpi-test.lisp"

cl-mpi-test:
	$(ECL) -load "build-mpi-test.lisp"
	mv -f cl-mpi-test-mono.asd $@

test-ecl-mpicc: cl-mpi-test
	mpirun -np $(NUMPROCS) `pwd`/cl-mpi-test

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
tarball: clean
	echo "Creating distribution tarball in parent directory.."
	rm -fr /tmp/cl-mpi
	cp -r $(CURRENT_DIR) /tmp
	rm -rf /tmp/cl-mpi/.svn*
	rm -rf /tmp/cl-mpi/*/.svn*
	tar -C /tmp -zcvf ../cl-mpi.tar.gz cl-mpi
clean:
	rm -f *svn-commit.tmp* *~ *.o *.fasl *.x86f *.sse2f mpi-grovel.c mpi-grovel.grovel-tmp.lisp mpi-grovel

