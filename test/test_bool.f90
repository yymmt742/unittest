program main
  use mod_unittest
  implicit none
  type(unittest)              :: u
  call u%init('Simple unittests', terminate_with_error_code=.false.)
  call u%assert(1 + 2 == 3, "test 1 :: 1 + 2 == 3")
  call u%assert(3 + 4 == 8, "test 2 :: 3 + 4 == 8")
  call u%finish_and_terminate()
end program main
