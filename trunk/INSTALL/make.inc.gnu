# Compiler for C and Fortran
# The Fortran compiler is only used if INCLAPACK is set to 1
CC = mpicc
FC = mpif90

# Compiler flags
CFLAGS = -pthread -O3
FFLAGS = -O3 -funderscoring

# Archiver and flags used when building the archive
AR = /usr/bin/ar 
ARFLAGS = rcs

# To build 'libmrrr.a' without adding necessary LAPACK routines 
# for the to the archive set value to 0; in this case linking 
# to LAPACK and BLAS is necessary; default value is 1
INCLAPACK = 1

# On some systems 'spinlocks' are not supported, therefore 
# here the flag to use 'mutexes' instead; default value is 1
SPINLOCK_SUPPORT = 1

