program main
  use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
  implicit none
  character(:), allocatable :: path
  integer                   :: u, l
  integer(INT64)            :: count_rate, count_max
  call GET_COMMAND_ARGUMENT(1, length=l)
  allocate (character(l)::path)
  call GET_COMMAND_ARGUMENT(1, path)
  open (NEWUNIT=u, FILE=path)
  call SYSTEM_CLOCK(count_rate=count_rate, count_max=count_max)
  write (u, '(A,I0, A)') "  real(RK), parameter       :: COUNT_RATE =  1.0_RK / REAL(", count_rate, "_INT64, RK)"
  write (u, '(A,I0, A)') "  integer(INT64), parameter :: COUNT_MAX  = ", count_max, "_INT64"
  close (u)
  deallocate (path)
end program main

