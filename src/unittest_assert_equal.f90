  subroutine utest_equal_byte_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a==b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] /= [', b, ']'
  end subroutine utest_equal_byte_00
!
  subroutine utest_equal_byte_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_equal_byte_01
!
  subroutine utest_equal_byte_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)==b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_equal_byte_10
!
  subroutine utest_equal_byte_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_equal_byte_11
!
  subroutine utest_not_equal_byte_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a/=b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] == [', b, ']'
  end subroutine utest_not_equal_byte_00
!
  subroutine utest_not_equal_byte_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_not_equal_byte_01
!
  subroutine utest_not_equal_byte_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)/=b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_not_equal_byte_10
!
  subroutine utest_not_equal_byte_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout ) :: this
  integer( INT8 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_not_equal_byte_11
!
!---------------------------------------------------------!
!
  subroutine utest_equal_int2_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a==b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] /= [', b, ']'
  end subroutine utest_equal_int2_00
!
  subroutine utest_equal_int2_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_equal_int2_01
!
  subroutine utest_equal_int2_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)==b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_equal_int2_10
!
  subroutine utest_equal_int2_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_equal_int2_11
!
  subroutine utest_not_equal_int2_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a/=b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] == [', b, ']'
  end subroutine utest_not_equal_int2_00
!
  subroutine utest_not_equal_int2_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_not_equal_int2_01
!
  subroutine utest_not_equal_int2_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)/=b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_not_equal_int2_10
!
  subroutine utest_not_equal_int2_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout ) :: this
  integer( INT16 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_not_equal_int2_11
!
!---------------------------------------------------------!
!
  subroutine utest_equal_int4_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a==b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] /= [', b, ']'
  end subroutine utest_equal_int4_00
!
  subroutine utest_equal_int4_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_equal_int4_01
!
  subroutine utest_equal_int4_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)==b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_equal_int4_10
!
  subroutine utest_equal_int4_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_equal_int4_11
!
  subroutine utest_not_equal_int4_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a/=b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] == [', b, ']'
  end subroutine utest_not_equal_int4_00
!
  subroutine utest_not_equal_int4_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_not_equal_int4_01
!
  subroutine utest_not_equal_int4_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)/=b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_not_equal_int4_10
!
  subroutine utest_not_equal_int4_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout ) :: this
  integer( INT32 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_not_equal_int4_11
!
!---------------------------------------------------------!
!
  subroutine utest_equal_int8_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a==b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] /= [', b, ']'
  end subroutine utest_equal_int8_00
!
  subroutine utest_equal_int8_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_equal_int8_01
!
  subroutine utest_equal_int8_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)==b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_equal_int8_10
!
  subroutine utest_equal_int8_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_equal_int8_11
!
  subroutine utest_not_equal_int8_00( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a/=b, unitname, err )
    if( err ) write( STDOUT, '(2A,I16,A,I16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] == [', b, ']'
  end subroutine utest_not_equal_int8_00
!
  subroutine utest_not_equal_int8_01( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a, '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_not_equal_int8_01
!
  subroutine utest_not_equal_int8_10( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)/=b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_not_equal_int8_10
!
  subroutine utest_not_equal_int8_11( this, a, b, unitname )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout ) :: this
  integer( INT64 ),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I16,A,I16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_not_equal_int8_11
!
!---------------------------------------------------------!
!
  subroutine utest_equal_char_00( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a==b, unitname, err )
    if( err ) write( STDOUT, '(2A,A,A,A,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] /= [', b, ']'
  end subroutine utest_equal_char_00
!
  subroutine utest_equal_char_01( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,A,A,A,A)', IOSTAT=ios ) i, '  xxx [', a, '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_equal_char_01
!
  subroutine utest_equal_char_10( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)==b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,A,A,A,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_equal_char_10
!
  subroutine utest_equal_char_11( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a==b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS == RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)==b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,A,A,A,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] /= [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_equal_char_11
!
  subroutine utest_not_equal_char_00( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a/=b, unitname, err )
    if( err ) write( STDOUT, '(2A,A,A,A,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] == [', b, ']'
  end subroutine utest_not_equal_char_00
!
  subroutine utest_not_equal_char_01( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,A,A,A,A)', IOSTAT=ios ) i, '  xxx [', a, '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_not_equal_char_01
!
  subroutine utest_not_equal_char_10( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i)/=b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,A,A,A,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_not_equal_char_10
!
  subroutine utest_not_equal_char_11( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  character(*),intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a/=b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS /= RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i)/=b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,A,A,A,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] == [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_not_equal_char_11
!
!---------------------------------------------------------!
!
  subroutine utest_equal_logi_00( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a.eqv.b, unitname, err )
    if( err ) write( STDOUT, '(2A,L16,A,L16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] .neqv. [', b, ']'
  end subroutine utest_equal_logi_00
!
  subroutine utest_equal_logi_01( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a.eqv.b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS .eqv. RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a.eqv.b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,L16,A,L16,A)', IOSTAT=ios ) i, '  xxx [', a, '] .neqv. [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_equal_logi_01
!
  subroutine utest_equal_logi_10( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a.eqv.b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS .eqv. RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i).eqv.b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,L16,A,L16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] .neqv. [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_equal_logi_10
!
  subroutine utest_equal_logi_11( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a.eqv.b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS .eqv. RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i).eqv.b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,L16,A,L16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] .neqv. [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_equal_logi_11
!
  subroutine utest_not_equal_logi_00( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a, b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, a.neqv.b, unitname, err )
    if( err ) write( STDOUT, '(2A,L16,A,L16,A)', IOSTAT=ios ) AssertionError,' xxx [', a, '] .eqv. [', b, ']'
  end subroutine utest_not_equal_logi_00
!
  subroutine utest_not_equal_logi_01( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a, b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a.neqv.b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS .neqv. RHS'
      nerr = 0
      do i=1,SIZE( b )
        if( a.neqv.b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,L16,A,L16,A)', IOSTAT=ios ) i, '  xxx [', a, '] .eqv. [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( b ), nerr )
    endif
  end subroutine utest_not_equal_logi_01
!
  subroutine utest_not_equal_logi_10( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a(:), b
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( a.neqv.b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS .neqv. RHS'
      nerr = 0
      do i=1,SIZE( a )
        if( a(i).neqv.b ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,L16,A,L16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] .eqv. [', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( a ), nerr )
    endif
  end subroutine utest_not_equal_logi_10
!
  subroutine utest_not_equal_logi_11( this, a, b, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: a(:), b(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    call utest_assert_printer( this, ALL( a.neqv.b ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'LHS .neqv. RHS'
      nerr = 0
      do i=1,MINVAL( [ SIZE( a ), SIZE( b ) ], 1 )
        if( a(i).neqv.b(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,L16,A,L16,A)', IOSTAT=ios ) i, '  xxx [', a(i), '] .eqv. [', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( MINVAL( [ SIZE( a ), SIZE( b ) ], 1 ), nerr )
    endif
  end subroutine utest_not_equal_logi_11
!
!---------------------------------------------------------!
!
!=========================================================!
!
