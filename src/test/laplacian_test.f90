      subroutine hpotcg_test(grid,pbc,parallel,elec_st)

      use constants
      use grid_module
      use pbc_module
      use parallel_data_module
      use electronic_struct_module

      implicit none
!
!     Input/Output variables:
!
      type (grid_data), intent(in) :: grid
      type (pbc_data), intent(in) :: pbc
      type (parallel_data), intent(in) :: parallel
      type (electronic_struct), intent(in) :: elec_st

!     work variables

      real(dp) :: pot_func(parallel%mydim), lapl(parallel%mydim), &
                  rho(parallel%mydim), pot_orig(parallel%mydim)
      real(dp) :: vec(parallel%nwedge+1)
      real(dp) :: u(3),x(3),gv(3),tmpvec(3),rx

      integer i,j,flag
      integer icellx,icelly,icellz

      real(dp) :: tmpmat(3,3),tmpdet,tmptr,rhoav
      real(dp) :: ainv(3,3),ainvdot(3,3)

      write(9,*) "entered hpotcg_test function" 

      tmpmat=pbc%avec_norm

      ainv=tmpmat

      call mtrxin(ainv,tmpdet,tmptr)

      ainvdot=matmul(transpose(ainv),ainv)

!     Producing the test function data!

      pot_func=0
      rho=0    
      flag=0

      do i=1,parallel%mydim

        if(flag>0) then
          u(1)=((grid%shift(1)+grid%kx(i))*grid%step(1))
          u(2)=((grid%shift(2)+grid%ky(i))*grid%step(2))
          u(3)=((grid%shift(3)+grid%kz(i))*grid%step(3))
          call matvec3('N',tmpmat,u,x)
!          x=matmul(tmpmat,u)
          gv(1)=4
          gv(2)=8
          gv(3)=5
          tmpvec(:)=gv(:)*u(:)/pbc%box_size(:)

          pot_func(i)=sin(2*pi*sum(tmpvec))


!        The analytical form of the laplacian of the sine function.

          tmpvec(:)=2*pi*gv(:)/pbc%box_size(:)
          call matvec3('N',ainvdot,tmpvec,gv)
          rho(i)=-dot_product(tmpvec,gv)*pot_func(i)
        else
          do icellx = -1,1
             do icelly = -1,1
                do icellz = -1,1

                   u(1)=(grid%shift(1) + grid%kx(i))* &
                         grid%step(1)+ &
                         real(icellx,dp)*pbc%box_size(1)
                   u(2)=(grid%shift(2) + grid%ky(i))* &
                         grid%step(2)+ &
                         real(icelly,dp)*pbc%box_size(2) 
                   u(3)=(grid%shift(3) + grid%kz(i))* &
                         grid%step(3)+ &
                         real(icellz,dp)*pbc%box_size(3)
                   call matvec3('N',tmpmat,u,x)
!                   x=matmul(tmpmat,u)
                   rx=sum(x(:)**2)

                   rho(i)=rho(i)+exp(-0.3*rx)
                 enddo
              enddo
          enddo
       endif    


      end do

      rho=100000*elec_st%rho(:,1)
      
      rhoav=sum(rho(:))/parallel%mydim

      rho=rho-rhoav

      pot_orig=pot_func
      rho=-rho ! doing the minus for rho so it will be a solution.

      pot_func=0 ! zeroing before call to hpotcg to make sure that
                 ! it is the calculated value that is being 
                 ! returned and not something that is left from
                 ! initialization.

      call hpotcg(parallel, rho, pot_func, grid%norder, grid%coe2, &
                        grid%lap_dir_num, vec)

      call lapmvs(parallel, pot_func, lapl, grid%coe2, grid%norder, &
                        grid%lap_dir_num, vec)
   

     

      call write_rho_1(parallel,pbc,grid,pot_func,rho,lapl)

      if(flag>0) then
       j=0
       write(9,*) 'hpotcg exception at:'
       do i=1,parallel%mydim
          if((abs(pot_orig(i))-abs(pot_func(i)))<0.0001) then
            j=j+1
          else
            u(1)=((grid%shift(1)+grid%kx(i))*grid%step(1))
            u(2)=((grid%shift(2)+grid%ky(i))*grid%step(2))
            u(3)=((grid%shift(3)+grid%kz(i))*grid%step(3))
           write(9,105) u, pot_orig(i), pot_func(i)
          endif
105    format(5(f15.9,1x))
       end do
       write(9,*) "number of diffs in pot: ", parallel%mydim-j
      endif


      j=0
      do i=1,parallel%mydim
         if(abs(lapl(i)-rho(i))<0.01) then
           j=j+1
         else
           u(1)=((grid%shift(1)+grid%kx(i))*grid%step(1))
           u(2)=((grid%shift(2)+grid%ky(i))*grid%step(2))
           u(3)=((grid%shift(3)+grid%kz(i))*grid%step(3))
           write(9,105) u, lapl(i), rho(i)
         endif
107   format(5(f15.9,1x))
      end do

      write(9,*) "number of diffs in rho: ",parallel%mydim-j

      end subroutine hpotcg_test

      subroutine write_rho_1(parallel,pbc,grid,vhart,rho,vhartcopy)

      use constants
      use cluster_module
      use grid_module
      use potential_module
      use pseudo_potential_module
      use parallel_data_module
      use pbc_module
      implicit none

      type (parallel_data), intent(in) :: parallel
      type (grid_data), intent(in) :: grid
      type (pbc_data), intent(in) :: pbc
!     potential related data
      real(dp), intent(in) :: vhart(parallel%mydim)
      real(dp), intent(in) :: rho(parallel%mydim)
      real(dp), intent(in) :: vhartcopy(parallel%mydim)


      integer i
      real(dp) :: u(3),x(3)
      real(dp) :: tmpmat(3,3)

      write(9,*) "entered write_rho_1 function"

      tmpmat=pbc%avec_norm

      open(unit=13,file='cgdatatest.dat',form='formatted')

      do i=1,parallel%mydim

         u(1)=(grid%shift(1)+grid%kx(i))*grid%step(1)
         u(2)=(grid%shift(2)+grid%ky(i))*grid%step(2)
         u(3)=(grid%shift(3)+grid%kz(i))*grid%step(3)
         call matvec3('N',tmpmat,u,x)
!         x=matmul(tmpmat,u)

         write(13,707) x(1),x(2),x(3),rho(i),vhart(i),vhartcopy(i)

      enddo

707   format(3(f15.9,','),e13.6,',',e13.6,',',e13.6)
      close(13)

      write(9,*) "end of write_rho_1 function"

      end subroutine write_rho_1

