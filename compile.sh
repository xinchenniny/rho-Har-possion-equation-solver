#!/bin/bash

# 删除旧的对象文件和模块文件
rm *.o *.mod

# 编译所有的 Fortran 源文件
gfortran -std=f2003 -c const.f90
gfortran -std=f2003 -c structures.F90
gfortran -std=f2003 -c inipbc.f90
gfortran -std=f2003 -c myflush.F90
gfortran -std=f2003 -c pbc_grid_coefs.f90
gfortran -std=f2003 -c comm_neigh.F90
gfortran -std=f2003 -c fourier_f.f90
gfortran -std=f2003 -c g_space.f90
gfortran -std=f2003 -c exit_err.F90
gfortran -std=f2003 -c cg.f90
gfortran -std=f2003 -c laplacian_test.f90
gfortran -std=f2003 -c hpotcg.F90
gfortran -std=f2003 -c main.F90
gfortran -std=f2003 -c symgen.f90

# 编译 C 文件
#gcc -c mycomplex.c

# 链接所有的对象文件
gfortran -std=f2003 -o my_program *.o -lblas

# 删除中间的对象文件（可选）
# rm -f *.o mycomplex.o
