module mod_unittest
  use, intrinsic :: ISO_FORTRAN_ENV, only: STDOUT => OUTPUT_UNIT, &
                  &                        STDERR => ERROR_UNIT, &
                  &                        RK => REAL64
  implicit none
  private
  public  :: unittest
!
  character(*), parameter :: SEP1 = REPEAT('-', 60)
  character(*), parameter :: SEP2 = REPEAT('=', 60)
  character(*), parameter :: SEP3 = REPEAT(' ', 8)//REPEAT('-', 36)
  character(*), parameter :: AssertionError = '        Assertion Error : '
  character(*), parameter :: LankMissMatchError = '   Lank MissMatch Error : '
  character(*), parameter :: ErrorRateIs = '          Error rate is : '
  integer, parameter      :: DEF_MP = 10**7
!
  type unittest
    private
    integer                   :: dev
    integer                   :: num_test
    integer                   :: num_error
    integer                   :: start_time
    real(RK)                  :: start_cpu_time
    character(:), allocatable :: script_name
    logical                   :: error_detected = .false.
  contains
    procedure         :: init => utest_init
    include "assert.h"
    include "equal.h"
    include "compare.h"
    include "almost_equal.h"
    procedure         :: finish => utest_finish
    procedure         :: finish_and_terminate => utest_finish_and_terminate
    final             :: utest_destroy
  end type unittest
!
  interface unittest
    module procedure :: utest_new
  end interface unittest
!
  interface
    include "assert.inc"
    include "compare.inc"
    include "almost_equal.inc"
  end interface
!
contains
!
! include "assert.f90"
  include "equal.f90"
! include "compare.f90"
! include "almost_equal.f90"
!
!=========================================================!
!
  function utest_new(section) result(res)
    character(*), intent(in), optional :: section
    type(unittest)                     :: res
    call utest_init(res, section)
  end function utest_new
!
  subroutine utest_init(this, section)
    class(unittest), intent(inout)     :: this
    character(*), intent(in), optional :: section
    integer                            :: lna, ios
    character(:), allocatable          :: tmp
    call utest_destroy(this)
!
    call CPU_TIME(this%start_cpu_time)
    call SYSTEM_CLOCK(this%start_time)
!
    call GET_COMMAND_ARGUMENT(0, length=lna, status=ios)
    if (ios /= 0) return
!
    this%dev = STDERR
    allocate (character(lna) :: tmp)
    allocate (character(0) :: this%script_name)
    call GET_COMMAND_ARGUMENT(0, value=tmp, status=ios)
    lna = INDEX(tmp, '/', .true.) + 1
    this%script_name = tmp(lna:)
    deallocate (tmp)
!
    write (this%dev, '(A)', err=100) SEP2
    write (this%dev, '(A)', err=100) 'Test command :: '//this%script_name
    if (PRESENT(section)) write (this%dev, '(A)', err=100) '  Section ----> '//section
!
100 return
  end subroutine utest_init
!
  subroutine utest_assert_lank_missmatch(this, size_a, size_b, unitname, err)
    class(unittest), intent(inout) :: this
    integer, intent(in)            :: size_a, size_b
    character(*), intent(in)       :: unitname
    logical, intent(inout)         :: err
    integer                        :: ios
!
    err = size_a /= size_b
!
    if (.not. ALLOCATED(this%script_name)) return
    if (.not. err) return
    this%num_test = this%num_test + 1
    this%num_error = this%num_error + 1
!
    write (this%dev, '(I8,A)', IOSTAT=ios) this%num_test, ' '//unitname//' ... failed ( lank miss match )'
    write (this%dev, '(2A,I0,A,I0,A)', IOSTAT=ios) LankMissMatchError, '[', size_a, '] /= [', size_b, ']'
    FLUSH (this%dev)
!
  end subroutine utest_assert_lank_missmatch
!
  subroutine utest_assert_printer(this, ok, unitname, err)
    class(unittest), intent(inout) :: this
    logical, intent(in)            :: ok
    character(*), intent(in)       :: unitname
    logical, intent(inout)         :: err
    integer                        :: ios
    err = .false.
    if (.not. ALLOCATED(this%script_name)) return
    err = .not. ok
!
    this%num_test = this%num_test + 1
!
    write (this%dev, '(I8,A)', ADVANCE='NO', IOSTAT=ios) this%num_test, ' '//unitname//' ... '
!
    if (ok) then
      write (this%dev, '(A)', IOSTAT=ios) 'ok'
    else
      write (this%dev, '(A)', IOSTAT=ios) 'failed'
      this%num_error = this%num_error + 1
    end if
!
    FLUSH (this%dev)
  end subroutine utest_assert_printer
!
  subroutine utest_error_rate_printer(this, ntest, nerror)
    class(unittest), intent(in) :: this
    integer, intent(in)         :: ntest, nerror
    real(RK)                    :: error_rate
    integer                     :: ios
    if (ntest < 1) return
    error_rate = real(nerror, RK) / real(ntest, RK)
    write (this%dev, '(A)', IOSTAT=ios) SEP3
    write (this%dev, '(A,f7.3,A)', IOSTAT=ios) ErrorRateIs, error_rate, ' %'
    FLUSH (this%dev)
  end subroutine utest_error_rate_printer
!
  subroutine utest_finish(this)
    class(unittest), intent(inout) :: this
    integer                        :: finish_time, t_rate, t_max
    real(RK)                       :: finish_cpu_time, time
!
    if (.not. ALLOCATED(this%script_name)) return
!
    call SYSTEM_CLOCK(finish_time, t_rate, t_max)
!
    if (finish_time < this%start_time) then
      time = (t_max - this%start_time + finish_time + 1)
    else
      time = (finish_time - this%start_time)
    end if
!
    call CPU_TIME(finish_cpu_time)
!
    finish_cpu_time = finish_cpu_time - this%start_cpu_time
!
    if (this%num_test > 0) then
      write (this%dev, '(A)', err=100) SEP1
      write (this%dev, '(A,I0,A,F9.3,A,F9.3,A)', advance='NO', err=100)    &
   &  ' Run ', this%num_test, ' tests in ', time / DBLE(t_rate), ' / ', &
   &  finish_cpu_time, ' s (sys/cpu)----> '
      if (this%num_error < 1) then
        write (this%dev, '(A,I0,A,F9.3,A)', err=100) 'Passed'
      else
        this%error_detected = .true.
        write (this%dev, '(A,/,I12,A)', err=100) 'Failed, ', this%num_error, ' error detected.'
      end if
    else
      write (this%dev, '(A)', err=100) 'No Test executed'
    end if
    write (this%dev, '(A)', err=100) SEP2
!
100 FLUSH (this%dev)
    call utest_free(this)
  end subroutine utest_finish
!
  subroutine utest_finish_and_terminate(this)
    class(unittest), intent(inout) :: this
    call utest_destroy(this)
    if (this%error_detected) ERROR stop 'TESTS WERE TERMINATED'
  end subroutine utest_finish_and_terminate
!
  pure subroutine utest_free(this)
    type(unittest), intent(inout) :: this
    if (ALLOCATED(this%script_name)) deallocate (this%script_name)
    this%num_test = 0
    this%num_error = 0
  end subroutine utest_free
!
  subroutine utest_destroy(this)
    type(unittest), intent(inout) :: this
    if (ALLOCATED(this%script_name)) call this%finish()
    call utest_free(this)
  end subroutine utest_destroy
!
end module mod_unittest
