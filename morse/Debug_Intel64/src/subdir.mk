################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/morse.f90 

OBJS += \
./src/morse.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: Intel(R) Intel(R) 64 Fortran Compiler'
	ifort -g -O0 -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/morse.o: ../src/morse.f90


