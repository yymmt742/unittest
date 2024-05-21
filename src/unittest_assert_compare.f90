  subroutine utest_less_byte_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_byte_00
!
  subroutine utest_less_byte_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_byte_01
!
  subroutine utest_less_byte_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_byte_10
!
  subroutine utest_less_byte_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_byte_11
!
  subroutine utest_less_equal_byte_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_byte_00
!
  subroutine utest_less_equal_byte_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_byte_01
!
  subroutine utest_less_equal_byte_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_byte_10
!
  subroutine utest_less_equal_byte_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_byte_11
!
  subroutine utest_greater_byte_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_byte_00
!
  subroutine utest_greater_byte_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_byte_01
!
  subroutine utest_greater_byte_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_byte_10
!
  subroutine utest_greater_byte_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_byte_11
!
  subroutine utest_greater_equal_byte_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_byte_00
!
  subroutine utest_greater_equal_byte_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_byte_01
!
  subroutine utest_greater_equal_byte_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_byte_10
!
  subroutine utest_greater_equal_byte_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT8
    class(unittest), intent(inout) :: this
    integer(INT8), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_byte_11
!
!---------------------------------------------------------!
!
  subroutine utest_less_int2_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_int2_00
!
  subroutine utest_less_int2_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_int2_01
!
  subroutine utest_less_int2_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_int2_10
!
  subroutine utest_less_int2_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_int2_11
!
  subroutine utest_less_equal_int2_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_int2_00
!
  subroutine utest_less_equal_int2_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_int2_01
!
  subroutine utest_less_equal_int2_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_int2_10
!
  subroutine utest_less_equal_int2_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_int2_11
!
  subroutine utest_greater_int2_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_int2_00
!
  subroutine utest_greater_int2_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_int2_01
!
  subroutine utest_greater_int2_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_int2_10
!
  subroutine utest_greater_int2_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_int2_11
!
  subroutine utest_greater_equal_int2_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_int2_00
!
  subroutine utest_greater_equal_int2_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_int2_01
!
  subroutine utest_greater_equal_int2_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_int2_10
!
  subroutine utest_greater_equal_int2_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT16
    class(unittest), intent(inout) :: this
    integer(INT16), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_int2_11
!
!---------------------------------------------------------!
!
  subroutine utest_less_int4_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_int4_00
!
  subroutine utest_less_int4_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_int4_01
!
  subroutine utest_less_int4_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_int4_10
!
  subroutine utest_less_int4_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_int4_11
!
  subroutine utest_less_equal_int4_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_int4_00
!
  subroutine utest_less_equal_int4_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_int4_01
!
  subroutine utest_less_equal_int4_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_int4_10
!
  subroutine utest_less_equal_int4_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_int4_11
!
  subroutine utest_greater_int4_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_int4_00
!
  subroutine utest_greater_int4_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_int4_01
!
  subroutine utest_greater_int4_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_int4_10
!
  subroutine utest_greater_int4_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_int4_11
!
  subroutine utest_greater_equal_int4_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_int4_00
!
  subroutine utest_greater_equal_int4_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_int4_01
!
  subroutine utest_greater_equal_int4_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_int4_10
!
  subroutine utest_greater_equal_int4_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT32
    class(unittest), intent(inout) :: this
    integer(INT32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_int4_11
!
!---------------------------------------------------------!
!
  subroutine utest_less_int8_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_int8_00
!
  subroutine utest_less_int8_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_int8_01
!
  subroutine utest_less_int8_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_int8_10
!
  subroutine utest_less_int8_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_int8_11
!
  subroutine utest_less_equal_int8_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_int8_00
!
  subroutine utest_less_equal_int8_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_int8_01
!
  subroutine utest_less_equal_int8_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_int8_10
!
  subroutine utest_less_equal_int8_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_int8_11
!
  subroutine utest_greater_int8_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_int8_00
!
  subroutine utest_greater_int8_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_int8_01
!
  subroutine utest_greater_int8_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_int8_10
!
  subroutine utest_greater_int8_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_int8_11
!
  subroutine utest_greater_equal_int8_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,I0,A,I0,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_int8_00
!
  subroutine utest_greater_equal_int8_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_int8_01
!
  subroutine utest_greater_equal_int8_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_int8_10
!
  subroutine utest_greater_equal_int8_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: INT64
    class(unittest), intent(inout) :: this
    integer(INT64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_int8_11
!
!---------------------------------------------------------!
!
  subroutine utest_less_real_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_real_00
!
  subroutine utest_less_real_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_real_01
!
  subroutine utest_less_real_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_real_10
!
  subroutine utest_less_real_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_real_11
!
  subroutine utest_less_equal_real_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_real_00
!
  subroutine utest_less_equal_real_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_real_01
!
  subroutine utest_less_equal_real_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_real_10
!
  subroutine utest_less_equal_real_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_real_11
!
  subroutine utest_greater_real_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_real_00
!
  subroutine utest_greater_real_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_real_01
!
  subroutine utest_greater_real_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_real_10
!
  subroutine utest_greater_real_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_real_11
!
  subroutine utest_greater_equal_real_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_real_00
!
  subroutine utest_greater_equal_real_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_real_01
!
  subroutine utest_greater_equal_real_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_real_10
!
  subroutine utest_greater_equal_real_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32
    class(unittest), intent(inout) :: this
    real(REAL32), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_real_11
!
!---------------------------------------------------------!
!
  subroutine utest_less_dble_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_dble_00
!
  subroutine utest_less_dble_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_dble_01
!
  subroutine utest_less_dble_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_dble_10
!
  subroutine utest_less_dble_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_dble_11
!
  subroutine utest_less_equal_dble_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_dble_00
!
  subroutine utest_less_equal_dble_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_dble_01
!
  subroutine utest_less_equal_dble_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_dble_10
!
  subroutine utest_less_equal_dble_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_dble_11
!
  subroutine utest_greater_dble_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_dble_00
!
  subroutine utest_greater_dble_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_dble_01
!
  subroutine utest_greater_dble_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_dble_10
!
  subroutine utest_greater_dble_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_dble_11
!
  subroutine utest_greater_equal_dble_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_dble_00
!
  subroutine utest_greater_equal_dble_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_dble_01
!
  subroutine utest_greater_equal_dble_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_dble_10
!
  subroutine utest_greater_equal_dble_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64
    class(unittest), intent(inout) :: this
    real(REAL64), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_dble_11
!
!---------------------------------------------------------!
!
  subroutine utest_less_quad_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a < b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] >= [', b, ']'
  end subroutine utest_less_quad_00
!
  subroutine utest_less_quad_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_quad_01
!
  subroutine utest_less_quad_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) < b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_quad_10
!
  subroutine utest_less_quad_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a < b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS < RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) < b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_quad_11
!
  subroutine utest_less_equal_quad_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a <= b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] > [', b, ']'
  end subroutine utest_less_equal_quad_00
!
  subroutine utest_less_equal_quad_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_less_equal_quad_01
!
  subroutine utest_less_equal_quad_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) <= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_less_equal_quad_10
!
  subroutine utest_less_equal_quad_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a <= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS <= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) <= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] >  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_less_equal_quad_11
!
  subroutine utest_greater_quad_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a > b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] <= [', b, ']'
  end subroutine utest_greater_quad_00
!
  subroutine utest_greater_quad_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_quad_01
!
  subroutine utest_greater_quad_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) > b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_quad_10
!
  subroutine utest_greater_quad_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a > b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS > RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) > b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <= [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_quad_11
!
  subroutine utest_greater_equal_quad_00(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: ios
    call utest_assert_printer(this, a >= b, unitname, err)
    if (err) write (STDOUT, '(2A,F16.9,A,F16.9,A)', IOSTAT=ios) AssertionError, ' xxx [', a, '] < [', b, ']'
  end subroutine utest_greater_equal_quad_00
!
  subroutine utest_greater_equal_quad_01(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a, b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(b)
        if (a >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a, '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(b), nerr)
    end if
  end subroutine utest_greater_equal_quad_01
!
  subroutine utest_greater_equal_quad_10(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, SIZE(a)
        if (a(i) >= b) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b, ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(SIZE(a), nerr)
    end if
  end subroutine utest_greater_equal_quad_10
!
  subroutine utest_greater_equal_quad_11(this, a, b, unitname)
    use, intrinsic :: ISO_FORTRAN_ENV, only: REAL128
    class(unittest), intent(inout) :: this
    real(REAL128), intent(in)      :: a(:), b(:)
    character(*), intent(in)       :: unitname
    logical                           :: err
    integer                          :: i, ios, nerr
    call utest_assert_lank_missmatch(this, SIZE(a), SIZE(b), unitname, err)
    if (err) return
    call utest_assert_printer(this, ALL(a >= b), unitname, err)
    if (err) then
      write (STDOUT, '(2A)', IOSTAT=ios) AssertionError, 'LHS >= RHS'
      nerr = 0
      do i = 1, MINVAL([SIZE(a), SIZE(b)], 1)
        if (a(i) >= b(i)) cycle; nerr = nerr + 1
        write (STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios) i, '  xxx [', a(i), '] <  [', b(i), ']'
        if (ios > 0) exit
      end do
      call utest_error_rate_printer(MINVAL([SIZE(a), SIZE(b)], 1), nerr)
    end if
  end subroutine utest_greater_equal_quad_11
!
!---------------------------------------------------------!
!
!=========================================================!
!
