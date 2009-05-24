
CC = mpicc
CFLAGS = -O3
default: libmpiskeleton.so


mpiskeleton.o : mpiskeleton.c
	mpicc -c -fPIC mpiskeleton.c -o mpiskeleton.o 
libmpiskeleton.so : mpiskeleton.o
	mpicc -shared  -o libmpiskeleton.so.1.0.1 -L/usr/local/lib -lmpich -lpthread -lrt


# make testc.so to create the stub binding
testc.o: testc.c
	gcc -c -fPIC testc.c -o testc.o

testc.so: testc.o
	gcc -shared -Wl,-soname,libtestc.so.1 -o libtestc.so.1.0.1 testc.o
clean:
	rm -f *.o *.so.*




