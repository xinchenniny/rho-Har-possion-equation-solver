!===============================================================
!
!  Copyright (C) 2005 Finite Difference Research Group
!  This file is part of parsec, http://www.ices.utexas.edu/parsec/
!
!  Defines:  
!      1. parallel%neibs in local form
!      2. irecvp,jsendp,senrows indices for exchanging
!         info in matvec
!      3. ip1,jp1 for exchanging info in preconditioning
!      4. pint local permutation with inter1,inter2 interior points
!
!  Rewrites parallel%neibs with a local one with the properties:
!
!              |--> parallel%neibs(neigh,row)-ioffset for all 
!              |     neighbors "neigh" local on this processor
!              |
!  parallel%neibs(neigh,row)= --> position of neigh, (> mydim) in the boundary 
!              |    values appended at the end of the current vector.
!              |    This is the "neigh" neighbor of the local "row" 
!              |   
!              |--> ndim+1 for all unused neighbors 
!
!  The boundary values needed from this node are padded at the
!  end of a local vector in a boundary-exchange information step.
!
!                     Receive and append info from 
!             local on the node    Proc1  Proc2  Proc4
!           <-------------------> <-----> <---> <------->
!
!  There is a local IP array : IP(i) = where the info from proc i
!  should be padded at. (eg IP(0) = ndim + 1, and IP(1) = ndim + 1
!  if nothing is received from proc 0). The rows that must be sent
!  to other processors are in senrows with index for each proc JP
!  Then matrix vector multiply is performed by exchanging info
!  filling the padded area and running the old mat-vec on it.
!
!  Also IP1 and JP1 are used for the communication in the 
!  preconditioning. This uses only the first shell (neighbors up
!  to norder) for every point. Since these are numbered first in
!  the boundary we put them in the same positions as in matvec.
!  Then parallel%neibs knows how to access them. Notice that IP
!  carries the positions of where they should be put. So we only
!  keep the sizes. Since we number the first shell first, the
!  senrows will contain exactly these rows if we take JP1(*) of them.
!
!  COMMUNICATION SIZE GRAPH:
!              Recv from 
!   local on the node    Proc1  Proc2  Proc4
! <-------------------> <-----> <---> <------->  index IP  (matvec)
!                       <->     <->   <-->    sizes in IP1 (precond)
!
!                  senrows
! <----------------------------------------->
! send Proc1     Proc2    Proc3      Proc4
! <-------><---------><------><------------->    index JP  (matvec)
! <-->     <-->       <->     <----->         sizes in JP1 (precond)
!
!----------------------------------------------------------------------
!  Experimentally sort first the interior rows (ie that do not 
!  require any neighbors from other nodes). Sort second the rows
!  that do not require any first shell neighbors. These will be
!  considered interior for the preconditioning. Permutation array
!  is pint and the above numbers inter1 and inter2 respectively.
!                        pint
!            <----------------------------------------->
!            <---------> inter1
!            <---------------------------> inter2
!
!	Algorithm
!  Count how many and which rows I need from each processor. (irecvp)
!  Also count the number of my rows each of my neighbors needs (jsendp)
!  (each time irow needs info from proc node, irow will be needed
!  by proc node -symmetric matrix-. If I have not received this
!  row already, add it in irecvp for reception. For sending out, a 
!  row should be sent only once to a node and not for all the 
!  neighbors of this row that reside on that node. To guarantee
!  that, for each node a flag array is updated for each row.
!  To keep the order of reception same to the one that other procs
!  will compute ---to retain locality in the access pattern of 
!     neighbors--- senrows is used as scratch to store the order.
!
!---------------------------------------------------------------
! Modified to eliminate local stack arrays.  MGR 16feb09
!---------------------------------------------------------------

!
!  Global sum of integer array
!
!---------------------------------------------------------------
subroutine pisum(ivec,nmax,nnodes,comm)

  use constants
  implicit none
  !
  !  Input/Output variables:
  !
  !  dimension of array vec
  integer, intent(in) :: nmax
  !  number of procs available
  integer, intent(in) :: nnodes
  !  communicator
  integer, intent(in) :: comm
  !  The original vector whose value is partial for each PE
  integer, intent(inout) :: ivec(nmax)
  !
  !  Work variables:
  !
  !  A temporary work array used to update the vectors
  integer, allocatable :: iwork(:)
  !  exit code for mpi calls
  integer mpinfo
  !  ---------------------------------------------------------------
  if (nnodes == 1) return

end subroutine pisum
!===============================================================
