# Compiler for C and Fortran
CC = mpicc
FC = mpif90

# Compiler flags
CFLAGS = -pthread -O3
FFLAGS = -O3 -funderscoring -fopenmp

# Archiver and flags used when building the archive
AR = /usr/bin/ar 
ARFLAGS = rcs

# On some systems 'spinlocks' are not supported, therefore 
# here the flag to use 'mutexes' instead; default value is 1
SPINLOCK_SUPPORT = 1

