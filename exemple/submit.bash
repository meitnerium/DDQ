#!/bin/bash 

#PBS -N 10_micron_2TVIBexact_cas_piegeant
#PBS -A cjt-923-aa 
#PBS -l walltime=03:00:00 
#PBS -l nodes=1:ppn=8
#PBS -M meitnerium109@gmail.com
#PBS -m bea
module load compilers/intel
module load mpi/openmpi
module load libs/mkl/11.1

cd $PBS_O_WORKDIR

mpirun -n 8 /home/fradion12/workspace/DDQ/bin/scanE
