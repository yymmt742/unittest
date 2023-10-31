  subroutine utest_almost_equal_byte_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = byte_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_byte_00
!
  subroutine utest_almost_equal_byte_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( byte_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_byte_01
!
  subroutine utest_almost_equal_byte_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( byte_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_byte_10
!
  subroutine utest_almost_equal_byte_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( byte_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_byte_11
!
  subroutine utest_not_almost_equal_byte_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = byte_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_byte_00
!
  subroutine utest_not_almost_equal_byte_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( byte_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_byte_01
!
  subroutine utest_not_almost_equal_byte_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( byte_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_byte_10
!
  subroutine utest_not_almost_equal_byte_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  class( unittest ),intent( inout )   :: this
  integer( INT8 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( byte_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_byte_11
!
!---------------------------------------------------------!
!
  pure elemental function byte_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
  integer( INT8 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function byte_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_int2_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = int2_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_int2_00
!
  subroutine utest_almost_equal_int2_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( int2_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int2_01
!
  subroutine utest_almost_equal_int2_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( int2_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int2_10
!
  subroutine utest_almost_equal_int2_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( int2_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int2_11
!
  subroutine utest_not_almost_equal_int2_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = int2_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_int2_00
!
  subroutine utest_not_almost_equal_int2_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( int2_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int2_01
!
  subroutine utest_not_almost_equal_int2_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( int2_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int2_10
!
  subroutine utest_not_almost_equal_int2_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  class( unittest ),intent( inout )   :: this
  integer( INT16 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( int2_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int2_11
!
!---------------------------------------------------------!
!
  pure elemental function int2_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT16
  integer( INT16 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function int2_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_int4_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = int4_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_int4_00
!
  subroutine utest_almost_equal_int4_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( int4_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int4_01
!
  subroutine utest_almost_equal_int4_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( int4_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int4_10
!
  subroutine utest_almost_equal_int4_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( int4_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int4_11
!
  subroutine utest_not_almost_equal_int4_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = int4_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_int4_00
!
  subroutine utest_not_almost_equal_int4_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( int4_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int4_01
!
  subroutine utest_not_almost_equal_int4_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( int4_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int4_10
!
  subroutine utest_not_almost_equal_int4_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  class( unittest ),intent( inout )   :: this
  integer( INT32 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( int4_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int4_11
!
!---------------------------------------------------------!
!
  pure elemental function int4_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT32
  integer( INT32 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function int4_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_int8_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = int8_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_int8_00
!
  subroutine utest_almost_equal_int8_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( int8_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int8_01
!
  subroutine utest_almost_equal_int8_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( int8_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int8_10
!
  subroutine utest_almost_equal_int8_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( int8_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_int8_11
!
  subroutine utest_not_almost_equal_int8_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = int8_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_int8_00
!
  subroutine utest_not_almost_equal_int8_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( int8_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int8_01
!
  subroutine utest_not_almost_equal_int8_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( int8_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int8_10
!
  subroutine utest_not_almost_equal_int8_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  class( unittest ),intent( inout )   :: this
  integer( INT64 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( int8_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,I0,A,I0,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_int8_11
!
!---------------------------------------------------------!
!
  pure elemental function int8_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : INT64
  integer( INT64 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function int8_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_real_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = real_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,F16.9,A,F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_real_00
!
  subroutine utest_almost_equal_real_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( real_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_real_01
!
  subroutine utest_almost_equal_real_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( real_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_real_10
!
  subroutine utest_almost_equal_real_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( real_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_real_11
!
  subroutine utest_not_almost_equal_real_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = real_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,F16.9,A,F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_real_00
!
  subroutine utest_not_almost_equal_real_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( real_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_real_01
!
  subroutine utest_not_almost_equal_real_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( real_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_real_10
!
  subroutine utest_not_almost_equal_real_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  real( REAL32 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( real_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_real_11
!
!---------------------------------------------------------!
!
  pure elemental function real_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  real( REAL32 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function real_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_dble_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = dble_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,F16.9,A,F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_dble_00
!
  subroutine utest_almost_equal_dble_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( dble_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_dble_01
!
  subroutine utest_almost_equal_dble_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( dble_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_dble_10
!
  subroutine utest_almost_equal_dble_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( dble_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_dble_11
!
  subroutine utest_not_almost_equal_dble_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = dble_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,F16.9,A,F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_dble_00
!
  subroutine utest_not_almost_equal_dble_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( dble_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_dble_01
!
  subroutine utest_not_almost_equal_dble_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( dble_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_dble_10
!
  subroutine utest_not_almost_equal_dble_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  real( REAL64 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( dble_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_dble_11
!
!---------------------------------------------------------!
!
  pure elemental function dble_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  real( REAL64 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function dble_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_quad_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = quad_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,F16.9,A,F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_quad_00
!
  subroutine utest_almost_equal_quad_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( quad_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_quad_01
!
  subroutine utest_almost_equal_quad_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( quad_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_quad_10
!
  subroutine utest_almost_equal_quad_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( quad_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_quad_11
!
  subroutine utest_not_almost_equal_quad_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = quad_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,F16.9,A,F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_quad_00
!
  subroutine utest_not_almost_equal_quad_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( quad_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_quad_01
!
  subroutine utest_not_almost_equal_quad_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( quad_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_quad_10
!
  subroutine utest_not_almost_equal_quad_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  real( REAL128),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( quad_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,F16.9,A,F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_quad_11
!
!---------------------------------------------------------!
!
  pure elemental function quad_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  real( REAL128),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function quad_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_cmp4_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = cmp4_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_cmp4_00
!
  subroutine utest_almost_equal_cmp4_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( cmp4_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp4_01
!
  subroutine utest_almost_equal_cmp4_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( cmp4_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp4_10
!
  subroutine utest_almost_equal_cmp4_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( cmp4_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp4_11
!
  subroutine utest_not_almost_equal_cmp4_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = cmp4_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_cmp4_00
!
  subroutine utest_not_almost_equal_cmp4_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( cmp4_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp4_01
!
  subroutine utest_not_almost_equal_cmp4_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( cmp4_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp4_10
!
  subroutine utest_not_almost_equal_cmp4_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  class( unittest ),intent( inout )   :: this
  complex( REAL32 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( cmp4_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp4_11
!
!---------------------------------------------------------!
!
  pure elemental function cmp4_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL32
  complex( REAL32 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function cmp4_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_cmp8_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = cmp8_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_cmp8_00
!
  subroutine utest_almost_equal_cmp8_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( cmp8_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp8_01
!
  subroutine utest_almost_equal_cmp8_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( cmp8_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp8_10
!
  subroutine utest_almost_equal_cmp8_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( cmp8_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp8_11
!
  subroutine utest_not_almost_equal_cmp8_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = cmp8_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_cmp8_00
!
  subroutine utest_not_almost_equal_cmp8_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( cmp8_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp8_01
!
  subroutine utest_not_almost_equal_cmp8_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( cmp8_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp8_10
!
  subroutine utest_not_almost_equal_cmp8_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  class( unittest ),intent( inout )   :: this
  complex( REAL64 ),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( cmp8_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp8_11
!
!---------------------------------------------------------!
!
  pure elemental function cmp8_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL64
  complex( REAL64 ),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function cmp8_almost_equal
!
!---------------------------------------------------------!
!
  subroutine utest_almost_equal_cmp16_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = cmp16_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud< 1, unitname, err )
    if( err ) write( STDOUT,'(2A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] >= [',b,']'
  end subroutine utest_almost_equal_cmp16_00
!
  subroutine utest_almost_equal_cmp16_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( cmp16_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a, ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp16_01
!
  subroutine utest_almost_equal_cmp16_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( cmp16_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp16_10
!
  subroutine utest_almost_equal_cmp16_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( cmp16_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud< 1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS == RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' xxx [', a(i), ']/=[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_almost_equal_cmp16_11
!
  subroutine utest_not_almost_equal_cmp16_00( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a, b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK )                       :: rud, ios
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    rud = cmp16_almost_equal( a, b, mp )
    call utest_assert_printer( this, rud>=1, unitname, err )
    if( err ) write( STDOUT,'(2A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) AssertionError, '[',a,'] < [',b,']'
  end subroutine utest_not_almost_equal_cmp16_00
!
  subroutine utest_not_almost_equal_cmp16_01( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a, b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( b )
    ALLOCATE( rud, source=[( cmp16_almost_equal( a, b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a, ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp16_01
!
  subroutine utest_not_almost_equal_cmp16_10( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a(:), b
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  SIZE( a )
    ALLOCATE( rud, source=[( cmp16_almost_equal( a(:), b, mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b, ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp16_10
!
  subroutine utest_not_almost_equal_cmp16_11( this, a, b, unitname, place )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  class( unittest ),intent( inout )   :: this
  complex( REAL128),intent( in )        :: a(:), b(:)
  character( * ),intent( in )         :: unitname
  integer( IK ),intent( in ),optional :: place
  real( RK )                          :: mp
  logical                             :: err
  integer( IK ),allocatable           :: rud(:)
  integer( IK )                       :: i, s, ios, nerr
    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )
    if( err ) RETURN
    if( PRESENT( place ) )then ; mp = 1D1**place
    else                       ; mp = DEF_MP
    endif
    s =  MINVAL( [ SIZE(a), SIZE(b) ], 1 )
    ALLOCATE( rud, source=[( cmp16_almost_equal( a(:), b(:), mp ), i=1,s )] )
    call utest_assert_printer( this, ALL( rud>=1 ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS /= RHS'
      nerr = 0
      do i=1,s
        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A,2F16.9,A,2F16.9,A)', IOSTAT=ios ) i, &
       & ' [', a(i), ']==[', b(i), ']'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( s, nerr )
    endif
    DEALLOCATE( rud )
  end subroutine utest_not_almost_equal_cmp16_11
!
!---------------------------------------------------------!
!
  pure elemental function cmp16_almost_equal( a, b, mp ) result( res )
  use,intrinsic :: ISO_FORTRAN_ENV, only : REAL128
  complex( REAL128),intent( in ) :: a,b
  real( RK ),intent( in )    :: mp
  real( RK )                 :: diff
  integer( IK )              :: res
    diff  = ABS( a - b )
    res   = INT( diff, IK )
    if( res>0 ) RETURN
    diff  = diff * mp
    res   = INT( diff, IK )
  end function cmp16_almost_equal
!
!---------------------------------------------------------!
!
!=========================================================!
!
