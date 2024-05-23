module mod_unittest_printer
!! Module for unittest output
  use, intrinsic :: ISO_FORTRAN_ENV, only: RK => REAL64
  implicit none
  private
  public  :: expr_report
  public  :: check_rank
  public  :: check_expr_all
  public  :: check_expr_not_any
!<&
  integer, parameter      :: L_MSG = 52
  integer, parameter      :: L_WDH = 40
  character(*), parameter :: WSPC  = REPEAT(' ', 8)
  character(*), parameter :: SEP3  = WSPC//REPEAT('-', L_MSG)
  character(*), parameter :: rankMissMatch = WSPC//'  Rank MissMatch : '
  character(*), parameter :: ErrorRateIs   = WSPC//'  Error rate is  : '
!
  character(*), parameter :: ESCAPE        = ACHAR(z'1b')
  character(*), parameter :: RED           = Escape//'[91m'
  character(*), parameter :: BLUE          = Escape//'[94m'
  character(*), parameter :: RESET         = Escape//'[0m'
!&>
  type expr_report
    sequence
    logical          :: ok
    real(RK)         :: error_rate
    character(L_MSG) :: msg
  end type expr_report
!
contains
  subroutine check_rank(dev, num_test, size_a, size_b, unitname, num_error, err)
    !!
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
    write (dev, '(I8,A)', IOSTAT=ios) num_test, padd_string(unitname, '... failed', L_WDH)
    write (dev, '(A)', IOSTAT=ios) SEP3
    write (dev, '(2A,I0,A,I0,A)', IOSTAT=ios) rankMissMatchError, '[', size_a, '] /= [', size_b, ']'
    write (dev, '(A)', IOSTAT=ios) SEP3
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
    if (err) then
      write (dev, '(I8,A)', IOSTAT=ios) num_test, padd_string(unitname, '... failed', L_WDH)
    else
      write (dev, '(I8,A)', IOSTAT=ios) num_test, padd_string(unitname, '... OK', L_WDH)
      return
    end if
!
    ntest = SIZE(expr)
!
    write (dev, '(A)', IOSTAT=ios) SEP3
    nerror = 0
    do i = 1, ntest
      if (expr(i)%ok) cycle
      write (dev, '(3X,I8,1X,A)', IOSTAT=ios) i, TRIM(expr(i)%msg)
      nerror = nerror + 1
    end do
!
    if (ntest < 2) return
    error_rate = 100.0_RK * real(nerror, RK) / real(ntest, RK)
    write (dev, '(A)', IOSTAT=ios) SEP3
    write (dev, '(A,f7.3,A,I8,A,I8,A)', IOSTAT=ios) ErrorRateIs, error_rate, ' %  (', nerror, ' /', ntest, ' )'
    FLUSH (dev)
  end subroutine report_result
!
  pure function padd_string(s, post, nline) result(res)
    character(*), intent(in)  :: s
    character(*), intent(in)  :: post
    integer, intent(in)       :: nline
    character(:), allocatable :: res
    integer                   :: npad
    npad = MAX(0, nline - LEN_TRIM(s))
    res = ' '//TRIM(s)//REPEAT(' ', npad)//post
  end function padd_string
end module mod_unittest_printer

