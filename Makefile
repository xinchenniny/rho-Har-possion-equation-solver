# Fortran and C compilers
FC = gfortran
CC = gcc

# Compiler flags
FFLAGS = -O2 -g -std=f2003
CFLAGS = -O2 -g
BLAS_LIBS = -L/Users/xinchenniny/coding/LAPACK/lapack-3.10.1/ -lblas 
LAPACK_LIBS = -L/Users/xinchenniny/coding/LAPACK/lapack-3.10.1 -llapack
INCLUDE_DIR = /Users/xinchenniny/coding/fortran-learning/my_try/1/include

# Define object files
OBJS = const.o structures.o myflush.o inipbc.o pbc_grid_coefs.o alccheck.o distdot.o cg.o fourier_f.o g_space.o symgen.o hpotcg.o laplacian_test.o main.o

# Define executable
EXEC = my_program

# Default rule to build everything
all: $(EXEC)

# Rule to build the executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $(EXEC) $(OBJS)

# Rules to compile Fortran files
const.o: const.f90
	$(FC) $(FFLAGS) -c const.f90

structures.o: structures.F90
	$(FC) $(FFLAGS) -c structures.F90

myflush.o: myflush.F90
	$(FC) $(FFLAGS) -c myflush.F90

inipbc.o: inipbc.f90
	$(FC) $(FFLAGS) -c inipbc.f90

pbc_grid_coefs.o: pbc_grid_coefs.f90
	$(FC) $(FFLAGS) -c pbc_grid_coefs.f90

alccheck.o: alccheck.f90
	$(FC) $(FFLAGS) -c alccheck.f90

distdot.o: distdot.f90
	$(FC) $(FFLAGS) -I$(INCLUDE_DIR) -c distdot.f90 -v


cg.o: cg.f90
	$(FC) $(FFLAGS) -c cg.f90

fourier_f.o: fourier_f.f90
	$(FC) $(FFLAGS) -c fourier_f.f90

g_space.o: g_space.f90
	$(FC) $(FFLAGS) -c g_space.f90

symgen.o: symgen.f90
	$(FC) $(FFLAGS) -c symgen.f90

hpotcg.o: hpotcg.f90
	$(FC) $(FFLAGS) -c hpotcg.f90

laplacian_test.o: laplacian_test.f90
	$(FC) $(FFLAGS) -c laplacian_test.f90

main.o: main.f90
	$(FC) $(FFLAGS) -c main.f90

# Clean up object files and executable
clean:
	rm -f *.o $(EXEC)

# Specify dependencies
depend: $(OBJS)
	$(FC) -MM $(OBJS) > Makefile.dep

-include Makefile.dep
