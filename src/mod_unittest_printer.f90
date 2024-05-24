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
  integer, parameter      :: L_MSG = 56
  integer, parameter      :: L_WDH = L_MSG - 12
  character(*), parameter :: ESCAPE        = ACHAR(z'1b')
  character(*), parameter :: BAR1          = Escape//'[91;40m█'
  character(*), parameter :: BAR2          = Escape//'[91;40m▓'
  character(*), parameter :: BAR3          = Escape//'[91;40m▒'
  character(*), parameter :: BAR4          = Escape//'[91;40m░'
  character(*), parameter :: BAR5          = Escape//'[31;40m░'
  character(*), parameter :: BAR6          = Escape//'[31;40m░'
  character(*), parameter :: BAR7          = Escape//'[31;40m░'
  character(*), parameter :: BAR8          = Escape//'[31;40m░'
  character(*), parameter :: BAR9          = Escape//'[35;40m░'
  character(*), parameter :: BAR10         = Escape//'[35;40m░'
  character(*), parameter :: BAR11         = Escape//'[0;30m█'
  character(*), parameter :: BAR12         = Escape//'[34;40m░'
  character(*), parameter :: BAR13         = Escape//'[34;40m░'
  character(*), parameter :: BAR14         = Escape//'[34;40m░'
  character(*), parameter :: BAR15         = Escape//'[34;40m░'
  character(*), parameter :: BAR16         = Escape//'[34;40m░'
  character(*), parameter :: BAR17         = Escape//'[34;40m░'
  character(*), parameter :: BAR18         = Escape//'[96;40m░'
  character(*), parameter :: BAR19         = Escape//'[96;40m▒'
  character(*), parameter :: BAR20         = Escape//'[96;40m▓'
  character(*), parameter :: BAR21         = Escape//'[96;40m█'
  character(*), parameter :: RESET         = Escape//'[m'
  character(*), parameter :: WSPC          = REPEAT(' ', 8)
  character(*), parameter :: SEP3          = WSPC//REPEAT('-', L_MSG)
  character(*), parameter :: rankMissMatch = WSPC//'  Rank MissMatch : '
  character(*), parameter :: ErrorRateIs   = WSPC//'  Error rate is  : '
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
    write (dev, '(2A,I0,A,I0,A)', IOSTAT=ios) rankMissMatch, '[', size_a, '] /= [', size_b, ']'
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
    nerror = COUNT(.not.expr%ok)
!
    if (ntest > 49) then
      call report_as_image(dev, 50, expr%error_rate)
    else
      do i = 1, ntest
        if (expr(i)%ok) cycle
        write (dev, '(3X,I8,1X,A)', IOSTAT=ios) i, TRIM(expr(i)%msg)
      end do
    end if
!
    if (ntest < 2) return
    error_rate = 100.0_RK * real(nerror, RK) / real(ntest, RK)
    write (dev, '(A)', IOSTAT=ios) SEP3
    write (dev, '(A,f7.3,A,I8,A,I8,A)', IOSTAT=ios) ErrorRateIs, error_rate, ' %  (', nerror, ' /', ntest, ' )'
!
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
!
  subroutine report_as_image(dev, nbreak, error_rate)
    integer, intent(in)  :: dev
    integer, intent(in)  :: nbreak
    real(RK), intent(in) :: error_rate(:)
    real(RK)             :: mmax, m1, m2, m3, m4, m5, m6, m7, m8, m9, m0
    integer              :: i, j, ios, nmap
    nmap = SIZE(error_rate)
    mmax = MAXVAL(ABS(error_rate))
    m1 = 0.05_RK * mmax
    m2 = 0.15_RK * mmax
    m3 = 0.25_RK * mmax
    m4 = 0.35_RK * mmax
    m5 = 0.45_RK * mmax
    m6 = 0.55_RK * mmax
    m7 = 0.65_RK * mmax
    m8 = 0.75_RK * mmax
    m9 = 0.85_RK * mmax
    m0 = 0.95_RK * mmax
    write (dev, '(14X,G12.3,A,G12.3)', IOSTAT=ios) &
   & -mmax, BAR1//BAR2//BAR3//BAR4//BAR5//BAR6//BAR7//BAR8//BAR9//BAR10//&
   & BAR11//BAR12//BAR13//BAR14//BAR15//BAR16//BAR17//BAR18//BAR19//BAR20//&
   & BAR21//RESET, mmax
    do i = 1, nmap, nbreak
      write (dev, '(I12,1X)', ADVANCE="NO", IOSTAT=ios) i
      do j = i, MIN(i + nbreak - 1, nmap)
        if (error_rate(j) < -m0) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR1
        elseif (error_rate(j) < -m9) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR2
        elseif (error_rate(j) < -m8) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR3
        elseif (error_rate(j) < -m7) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR4
        elseif (error_rate(j) < -m6) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR5
        elseif (error_rate(j) < -m5) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR6
        elseif (error_rate(j) < -m4) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR7
        elseif (error_rate(j) < -m3) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR8
        elseif (error_rate(j) < -m2) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR9
        elseif (error_rate(j) < -m1) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR10
        elseif (error_rate(j) < m1) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR11
        elseif (error_rate(j) < m2) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR12
        elseif (error_rate(j) < m3) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR13
        elseif (error_rate(j) < m4) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR14
        elseif (error_rate(j) < m5) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR15
        elseif (error_rate(j) < m6) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR16
        elseif (error_rate(j) < m7) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR17
        elseif (error_rate(j) < m8) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR18
        elseif (error_rate(j) < m9) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR19
        elseif (error_rate(j) < m0) then
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR20
        else
          write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) BAR21
        end if
      end do
      write (dev, '(A)', IOSTAT=ios) RESET
    end do
  end subroutine report_as_image
!
end module mod_unittest_printer

