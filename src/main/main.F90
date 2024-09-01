program main
    use constants
    use grid_module
    use pbc_module
    use parallel_data_module
    use electronic_struct_module
    implicit none

    ! Declare variables
    type(grid_data) :: grid
    type(pbc_data) :: pbc
    type(parallel_data) :: parallel
    type(electronic_struct) :: elec_st

    ! Initialize variables
    call init_grid(grid) 
    call init_pbc(pbc) !also sets pbc%is_on=.false.
    call init_parallel_data(parallel) 
    call init_electronic_struct(elec_st)

    ! Call the hpotcg_test subroutine
    call hpotcg_test(grid, pbc, parallel, elec_st)

    ! End the program
    print *, "Test completed."

end program main
