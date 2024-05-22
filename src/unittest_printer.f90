module mod_unittest_printer
  use, intrinsic :: ISO_FORTRAN_ENV, only: RK => REAL64
  implicit none
  private
  public  :: expr_report
  public  :: check_rank
  public  :: check_expr_all
  public  :: check_expr_not_any
!
  integer, parameter      :: L_MSG = 52
  character(*), parameter :: WSPC = REPEAT(' ', 8)
  character(*), parameter :: SEP3 = WSPC//REPEAT('-', L_MSG)
  character(*), parameter :: rankMissMatchError = '   rank MissMatch Error : '
  character(*), parameter :: ErrorRateIs = '          Error rate is : '
!
  type expr_report
    sequence
    logical          :: ok
    real(RK)         :: error_rate
    character(L_MSG) :: msg
  end type expr_report
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
    integer, intent(in)           :: dev
    integer, intent(in)           :: num_test
    type(expr_report), intent(in) :: expr(:)
    character(*), intent(in)      :: unitname
    integer, intent(inout)        :: num_error
    logical, intent(inout)        :: err
    err = .not. ALL(expr%ok)
    if (err) num_error = num_error + 1
    call report_result(dev, num_test, err, expr, unitname)
  end subroutine check_expr_all
!
  subroutine check_expr_not_any(dev, num_test, expr, unitname, num_error, err)
    integer, intent(in)           :: dev
    integer, intent(in)           :: num_test
    type(expr_report), intent(in) :: expr(:)
    character(*), intent(in)      :: unitname
    integer, intent(inout)        :: num_error
    logical, intent(inout)        :: err
    err = ANY(expr%ok)
    if (err) num_error = num_error + 1
    call report_result(dev, num_test, err, expr, unitname)
  end subroutine check_expr_not_any
!
  subroutine report_result(dev, num_test, err, expr, unitname)
    integer, intent(in)           :: dev
    integer, intent(in)           :: num_test
    logical, intent(in)           :: err
    type(expr_report), intent(in) :: expr(:)
    character(*), intent(in)      :: unitname
    real(RK)                      :: error_rate
    integer                       :: i, nerror, ntest, ios
!
    write (dev, '(I8,A)', ADVANCE='NO', IOSTAT=ios) num_test, ' '//unitname//' ... '
    if (err) then
      write (dev, '(A)', IOSTAT=ios) 'failed'
    else
      write (dev, '(A)', IOSTAT=ios) 'ok'
      return
    end if
!
    ntest = SIZE(expr)
!
    nerror = 0
    do i = 1, ntest
      if (expr(i)%ok) cycle
      write (dev, '(6X,I8,A)', IOSTAT=ios) i, TRIM(expr(i)%msg)
      nerror = nerror + 1
    end do
!
    if (ntest < 2) return
    error_rate = real(nerror, RK) / real(ntest, RK)
    write (dev, '(A)', IOSTAT=ios) SEP3
    write (dev, '(A,f7.3,A)', IOSTAT=ios) ErrorRateIs, error_rate, ' %'
    FLUSH (dev)
  end subroutine report_result
!
end module mod_unittest_printer

