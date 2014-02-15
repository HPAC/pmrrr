# Compiler for C and Fortran
CC = $(MPICC)
FC = $(MPIF77)

# Compiler flags
CFLAGS = -pthread -O3 -restrict
FFLAGS = -nofor_main -O3 -assume underscore -openmp

# Archiver and flags used when building the archive
AR = /usr/bin/ar 
ARFLAGS = rcs

# On some systems 'spinlocks' are not supported, therefore 
# here the flag to use 'mutexes' instead; default value is 1
SPINLOCK_SUPPORT = 1