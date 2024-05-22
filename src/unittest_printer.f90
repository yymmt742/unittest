module mod_unittest_printer
  use, intrinsic :: ISO_FORTRAN_ENV, only: RK => REAL64
  implicit none
  private
  public  :: report_result
  public  :: report_lank_missmatch
  public  :: report_error_rate
!
  character(*), parameter :: SEP3 = REPEAT(' ', 8)//REPEAT('-', 36)
  character(*), parameter :: LankMissMatchError = '   Lank MissMatch Error : '
  character(*), parameter :: ErrorRateIs = '          Error rate is : '
!
contains
!
  subroutine report_lank_missmatch(dev, size_a, size_b, unitname, num_test, num_error, err)
    integer, intent(in)      :: dev, size_a, size_b
    character(*), intent(in) :: unitname
    integer, intent(inout)   :: num_test, num_error
    logical, intent(inout)   :: err
    integer                  :: ios
    err = size_a /= size_b; if (.not. err) return
    num_test = num_test + 1
    num_error = num_error + 1
    write (dev, '(I8,A)', IOSTAT=ios) num_test, ' '//unitname//' ... failed ( lank miss match )'
    write (dev, '(2A,I0,A,I0,A)', IOSTAT=ios) LankMissMatchError, '[', size_a, '] /= [', size_b, ']'
    FLUSH (dev)
  end subroutine report_lank_missmatch
!
  subroutine report_result(dev, expr_is_true, unitname, num_test, num_error, err)
    integer, intent(in)      :: dev
    logical, intent(in)      :: expr_is_true
    character(*), intent(in) :: unitname
    integer, intent(inout)   :: num_test, num_error
    logical, intent(inout)   :: err
    integer                  :: ios
    err = .not. expr_is_true
    num_test = num_test + 1
    write (dev, '(I8,A)', ADVANCE='NO', IOSTAT=ios) num_test, ' '//unitname//' ... '
    if (expr_is_true) then
      write (dev, '(A)', IOSTAT=ios) 'ok'
    else
      write (dev, '(A)', IOSTAT=ios) 'failed'
      num_error = num_error + 1
    end if
    FLUSH (dev)
  end subroutine report_result
!
  subroutine report_error_rate(dev, ntest, nerror)
    integer, intent(in)         :: dev, ntest, nerror
    real(RK)                    :: error_rate
    integer                     :: ios
    if (ntest < 1) return
    error_rate = real(nerror, RK) / real(ntest, RK)
    write (dev, '(A)', IOSTAT=ios) SEP3
    write (dev, '(A,f7.3,A)', IOSTAT=ios) ErrorRateIs, error_rate, ' %'
    FLUSH (dev)
  end subroutine report_error_rate
!
end module mod_unittest_printer

