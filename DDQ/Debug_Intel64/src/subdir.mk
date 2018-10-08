################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/asymp.f90 \
../src/ddq.f90 \
../src/spo_maj.f90 

F_SRCS += \
../src/cmplxmath.f 

OBJS += \
./src/asymp.o \
./src/cmplxmath.o \
./src/ddq.o \
./src/spo_maj.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: Intel(R) Intel(R) 64 Fortran Compiler'
	ifort -g -O0 -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/asymp.o: ../src/asymp.f90

src/%.o: ../src/%.f
	@echo 'Building file: $<'
	@echo 'Invoking: Intel(R) Intel(R) 64 Fortran Compiler'
	ifort -g -O0 -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/cmplxmath.o: ../src/cmplxmath.f

src/ddq.o: ../src/ddq.f90
	@echo 'Building file: $<'
	@echo 'Invoking: Intel(R) Intel(R) 64 Fortran Compiler'
	ifort -g -O0 -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/ddq.o: ../src/ddq.f90

src/spo_maj.o: ../src/spo_maj.f90


