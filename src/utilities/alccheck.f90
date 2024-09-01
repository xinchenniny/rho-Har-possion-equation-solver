!===============================================================
!
! Checks allocation of array with size size, name wst. If
! allocation fails, an error message is writen in out.* file and
! the calculation is aborted in an abrupt and possibly nasty way.
!
! All "big arrays" (i.e., with size proportional to grid size or
! square of number of eigenvalues) should have allocation checked.
!
!---------------------------------------------------------------
subroutine alccheck(wst,isize,istat)
  implicit none
  !
  ! Input/Output variables:
  !
  integer, intent(in) :: isize, istat
  character (len=*), intent(in) :: wst
  !
  ! Work variables:
  !
  logical :: ltest          ! true if unit 7 is open
  integer :: aborterr       ! just for completness of mpi call
  if(istat /= 0) then
     write(9,'(a,a)') ' *** MEMORY ALLOCATION FAILED ON ARRAY ',wst
     write(9,'(a,i12)') '     ARRAY SIZE: ',isize
     call myflush(9)
     inquire(unit=7,opened=ltest)
     if (ltest) then
        write(7,'(a,a)') ' *** MEMORY ALLOCATION FAILED ON ARRAY ',wst
        write(7,'(a,i12)') '     ARRAY SIZE: ',isize
        call myflush(7)
     endif
     stop
  endif

end subroutine alccheck
!===============================================================
