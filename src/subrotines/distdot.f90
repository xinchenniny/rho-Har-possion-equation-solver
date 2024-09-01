#include "mycomplex.h"

function distdot(n,x,ix,y,iy,nnodes,comm)

  use constants
  implicit none
  !
  !  Input/Output variables:
  !
  integer, intent(in) :: n, ix, iy, nnodes, comm
  real(dp), intent(in) :: x(n), y(n)
  !  dot product
  real(dp) :: distdot
  !
  !  Work variables:
  !
  real(dp) :: d
  !
  !  External functions:
  !
  real(dp), external :: ddot
  !---------------------------------------------------------------
  d = ddot(n,x,ix,y,iy)
  call psum(d,1,nnodes,comm)
  distdot = d

end function distdot