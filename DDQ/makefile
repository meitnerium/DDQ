#FC=ifort
FC=gfortran
FOPTS= 

all: bin/ddq 

bin/ddq: src/ddq.f90 asymp.o spo_maj.o cmplxmath.o  
	$(FC) $(FOPTS) asymp.o spo_maj.o cmplxmath.o src/ddq.f90  -o bin/ddq

spo_maj.o : src/spo_maj.f90 cmplxmath.o  
	$(FC) $(FOPTS) cmplxmath.o -c src/spo_maj.f90

cmplxmath.o: src/cmplxmath.f
	$(FC) $(FOPTS) -c src/cmplxmath.f

asymp.o:	src/asymp.f90 spo_maj.o
	$(FC) $(FOPTS) -c src/asymp.f90

run: bin/ddq
	./bin/ddq

clean:
	rm bin/ddq *.o


