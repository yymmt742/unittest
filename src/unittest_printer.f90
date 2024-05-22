module mod_unittest_printer
  use, intrinsic :: ISO_FORTRAN_ENV, only: RK => REAL64
  implicit none
  private
  public  :: check_rank
  public  :: check_expr_all
  public  :: check_expr_not_any
  public  :: report_summary
!
  character(*), parameter :: SEP3 = REPEAT(' ', 8)//REPEAT('-', 36)
  character(*), parameter :: rankMissMatchError = '   rank MissMatch Error : '
  character(*), parameter :: ErrorRateIs = '          Error rate is : '
!
contains
  subroutine check_rank(dev, num_test, size_a, size_b, unitname, num_error, err)
    integer, intent(in)      :: dev
    integer, intent(in)      :: num_test
    integer, intent(in)      :: size_a
    integer, intent(in)      :: size_b
    character(*), intent(in) :: unitname
    integer, intent(inout)   :: num_error
    logical, intent(inout)   :: err
    integer                  :: ios
    err = size_a /= size_b; if (.not. err) return
    num_error = num_error + 1
    write (dev, '(I8,A)', IOSTAT=ios) num_test, ' '//unitname//' ... failed ( rank miss match )'
    write (dev, '(2A,I0,A,I0,A)', IOSTAT=ios) rankMissMatchError, '[', size_a, '] /= [', size_b, ']'
    FLUSH (dev)
  end subroutine check_rank
!
  subroutine check_expr_all(dev, num_test, expr, unitname, num_error, err)
    integer, intent(in)      :: dev
    integer, intent(in)      :: num_test
    logical, intent(in)      :: expr(:)
    character(*), intent(in) :: unitname
    integer, intent(inout)   :: num_error
    logical, intent(inout)   :: err
    err = .not. ALL(expr)
    if (err) num_error = num_error + 1
    call report_result(dev, num_test, err, unitname)
  end subroutine check_expr_all
!
  subroutine check_expr_not_any(dev, num_test, expr, unitname, num_error, err)
    integer, intent(in)      :: dev
    integer, intent(in)      :: num_test
    logical, intent(in)      :: expr(:)
    character(*), intent(in) :: unitname
    integer, intent(inout)   :: num_error
    logical, intent(inout)   :: err
    err = ANY(expr)
    if (err) num_error = num_error + 1
    call report_result(dev, num_test, err, unitname)
  end subroutine check_expr_not_any
!
  subroutine report_result(dev, num_test, err, unitname)
    integer, intent(in)      :: dev
    integer, intent(in)      :: num_test
    logical, intent(in)      :: err
    character(*), intent(in) :: unitname
    integer                  :: ios
    write (dev, '(I8,A)', ADVANCE='NO', IOSTAT=ios) num_test, ' '//unitname//' ... '
    if (err) then
      write (dev, '(A)', IOSTAT=ios) 'failed'
    else
      write (dev, '(A)', IOSTAT=ios) 'ok'
    end if
    FLUSH (dev)
  end subroutine report_result
!
  subroutine report_summary(dev, expr, error_rate)
    integer, intent(in)      :: dev
    logical, intent(in)      :: expr(:)
    real(RK), intent(in)     :: error_rate(:)
    integer                  :: i, nerror, ios
    nerror = 0
    do i = 1, SIZE(expr)
      if (expr(i)) cycle
      write (dev, '(6X,I8,A)', IOSTAT=ios) i, '  xxx'
      nerror = nerror + 1
    end do
    call report_error_rate(dev, SIZE(expr), nerror)
    FLUSH (dev)
  end subroutine report_summary
!
  subroutine report_error_rate(dev, ntest, nerror)
    integer, intent(in)         :: dev, ntest, nerror
    real(RK)                    :: error_rate
    integer                     :: ios
    if (ntest < 2) return
    error_rate = real(nerror, RK) / real(ntest, RK)
    write (dev, '(A)', IOSTAT=ios) SEP3
    write (dev, '(A,f7.3,A)', IOSTAT=ios) ErrorRateIs, error_rate, ' %'
    FLUSH (dev)
  end subroutine report_error_rate
!
end module mod_unittest_printer

