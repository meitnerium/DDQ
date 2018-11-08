FC=gfortran
FOPTS=-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan

all: x run clean 

run: x
	./x
	python see.py

clean:
	rm x *.o *.mod fort.* see.py

x: ddq.f90 asymp.o spo_maj.o math.o basics.o cmplxmath.f diag.f morse.o potspec_maj.f
	$(FC) $(FOPTS) -o x ddq.f90 asymp.o spo_maj.o math.o basics.o cmplxmath.f diag.f morse.o potspec_maj.f

asymp.o: asymp.f90 spo_maj.o math.o basics.o
	$(FC) $(FOPTS) -c asymp.f90 spo_maj.o math.o basics.o

spo_maj.o: spo_maj.f90 math.o basics.o
	$(FC) $(FOPTS) -c spo_maj.f90 math.o basics.o

math.o: math.f90 basics.o
	$(FC) $(FOPTS) -c math.f90 basics.o

basics.o: basics.f90
	$(FC) $(FOPTS) -c basics.f90

morse.o: morse.f90
	$(FC) $(FOPTS) -c morse.f90
