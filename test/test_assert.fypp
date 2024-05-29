program main
  use mod_unittest
  implicit none
  type(unittest) :: u
  logical        :: is_failed
  call u%init('Simple unittests', terminate_with_error_code=.false.)
  call u%assert(1 + 2 == 3, is_failed=is_failed)
  if (is_failed) call error_stop(code=1)
  call u%assert(3 + 4 == 8, is_failed=is_failed)
  if (.not. is_failed) call error_stop(code=1)
  call u%finish_and_terminate()
end program main
