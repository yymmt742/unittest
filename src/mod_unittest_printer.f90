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
  character(*), parameter :: ESC   = ACHAR(z'1b')//"["
!
  integer, parameter      :: NCMAP = 21
  character(*), parameter :: CDIV(NCMAP)   = ["164", "129", "093", "092", "056", &
                           &                  "055", "054", "236", "234", "232", &
                           &                  "000", "232", "234", "022", "023", &
                           &                  "031", "037", "036", "041", "046", &
                           &                  "118"]
! character(*), parameter :: CDIV(NCMAP)   = ["165", "129", "093", "057", "021", &
!                          &                  "020", "019", "018", "017", "016", &
!                          &                  "000", "052", "088", "124", "160", &
!                          &                  "196", "202", "208", "214", "220", &
!                          &                  "226"]
  character(*), parameter :: CSEQ(NCMAP)   = ["232", "233", "234", "235", "236", &
                           &                  "237", "238", "239", "240", "241", &
                           &                  "242", "243", "244", "245", "246", &
                           &                  "247", "248", "249", "251", "253", &
                           &                  "255"]
  character(*), parameter :: CSEQ_R(NCMAP) = ["255", "253", "251", "249", "248", &
                           &                  "247", "246", "245", "244", "243", &
                           &                  "242", "241", "240", "239", "238", &
                           &                  "237", "236", "235", "234", "233", &
                           &                  "232"]
  character(*), parameter :: RESET         = Esc//'m'
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
    if (ntest < 100) then
      do i = 1, ntest
        if (expr(i)%ok) cycle
        write (dev, '(3X,I8,1X,A)', IOSTAT=ios) i, TRIM(expr(i)%msg)
      end do
    else
      call report_as_image(dev, ntest, expr%error_rate)
    end if
!
    if (ntest > 1) then
      error_rate = 100.0_RK * real(nerror, RK) / real(ntest, RK)
      write (dev, '(A)', IOSTAT=ios) SEP3
      write (dev, '(A,f7.3,A,I0,A,I0,A)', IOSTAT=ios) &
     &  ErrorRateIs, error_rate, ' %  ( ', nerror, ' / ', ntest, ' )'
    end if
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
  subroutine report_as_image(dev, ndat, error_rate)
    integer, intent(in)  :: dev, ndat
    real(RK), intent(in) :: error_rate(ndat)
    real(RK)             :: mmax
    integer              :: nmap
    nmap = SIZE(error_rate)
    mmax = MAXVAL(ABS(error_rate))
    if (ALL(error_rate >= 0.0_RK)) then
      call dump_image(dev, ndat, SIZE(CSEQ), 100, error_rate, 0.0_RK, mmax, CSEQ)
    elseif (ALL(error_rate <= 0.0_RK)) then
      call dump_image(dev, ndat, SIZE(CSEQ_R), 100, error_rate, -mmax, 0.0_RK, CSEQ_R)
    else
      call dump_image(dev, ndat, SIZE(CDIV), 100, error_rate, -mmax, mmax, CDIV)
    end if
  end subroutine report_as_image
!
  subroutine dump_image(dev, ndat, nmap, nbreak, dat, mmin, mmax, cmap)
    integer, intent(in)       :: dev, ndat, nmap, nbreak
    real(RK), intent(in)      :: dat(ndat), mmin, mmax
    character(*), intent(in)  :: cmap(:)
    real(RK)                  :: norm
    integer                   :: i, j, k1, k2, n, ios
    norm = (nmap - 1) / (mmax - mmin)
    write (dev, '(14X,G12.3,A,G12.3)', IOSTAT=ios) mmin, cbar(cmap), mmax
    do i = 1, ndat, nbreak
      n = MIN(i + nbreak - 1, ndat)
      write (dev, '(I6,A,I6, 1X)', ADVANCE="NO", IOSTAT=ios) i, "-", n
      do j = i + 1, n, 2
        k1 = NINT(norm * (dat(j - 1) - mmin)) + 1
        k2 = NINT(norm * (dat(j - 0) - mmin)) + 1
        write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) cset(cmap(k1), cmap(k2))//"▌"
      end do
      if (MODULO(n, 2) == 1) then
        k1 = NINT(norm * (dat(n) - mmin)) + 1
        write (dev, '(A)', ADVANCE="NO", IOSTAT=ios) cset(cmap(k1))//"▌"
      end if
      write (dev, '(A)', IOSTAT=ios) RESET
    end do
  end subroutine dump_image
!
  pure function cset(C1, C2) result(res)
    character(*), intent(in)           :: C1
    character(*), intent(in), optional :: C2
    integer, parameter                 :: LCSET = LEN(ESC//"38;5;000;48;5;000m")
    character(LCSET)                   :: res
    if (PRESENT(C2)) then
      res = ESC//"38;5;"//C1//";48;5;"//C2//"m"
    else
      res = ESC//"38;5;"//C1//";48;5;000m"
    end if
  end function cset
!
  pure function cbar(cmap) result(res)
    character(*), intent(in)  :: cmap(:)
    character(:), allocatable :: res
    integer                   :: i, n
    res = ""
    n = SIZE(cmap)
    do i = 2, n, 2
      res = res//cset(cmap(i-1), cmap(i))//"▌"
    end do
    if(MODULO(n, 2)==1)then
      res = res//cset(cmap(n), "000")//"▌"
    endif
    res = res//RESET
  end function cbar
!
end module mod_unittest_printer

