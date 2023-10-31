  subroutine utest_assert_0( this, ok, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: ok
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, ok, unitname, err )
    if( err ) write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,' xxx ASSERT ERROR'
  end subroutine utest_assert_0
!
  subroutine utest_assert_1( this, ok, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: ok(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( ok ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'ASSERT 1D'
      nerr = 0
      do i=1,SIZE( ok )
        if( .not.ok(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A)', IOSTAT=ios ) i, '  xxx error'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( ok ), nerr )
    endif
  end subroutine utest_assert_1
!
  subroutine utest_assert_true_0( this, ok, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: ok
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, ok, unitname, err )
    if( err ) write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,' xxx ASSERT_TRUE ERROR'
  end subroutine utest_assert_true_0
!
  subroutine utest_assert_true_1( this, ok, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: ok(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, ALL( ok ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'ASSERT_TRUE 1D'
      nerr = 0
      do i=1,SIZE( ok )
        if( .not.ok(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A)', IOSTAT=ios ) i, '  xxx error'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( ok ), nerr )
    endif
  end subroutine utest_assert_true_1
!
  subroutine utest_assert_false_0( this, ok, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: ok
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: ios
    call utest_assert_printer( this, .not.ok, unitname, err )
    if( err ) write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,' xxx ASSERT_FALSE ERROR'
  end subroutine utest_assert_false_0
!
  subroutine utest_assert_false_1( this, ok, unitname )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )      :: ok(:)
  character( * ),intent( in )       :: unitname
  logical                           :: err
  integer( IK )                     :: i, ios, nerr
    call utest_assert_printer( this, .not.ANY( ok ), unitname, err )
    if( err )then
      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, 'ASSERT_FALSE 1D'
      nerr = 0
      do i=1,SIZE( ok )
        if( .not.ok(i) ) CYCLE ; nerr = nerr + 1
        write( STDOUT, '(6X,i8,A)', IOSTAT=ios ) i, '  xxx error'
        if( ios>0 ) EXIT
      enddo
      call utest_error_rate_printer( SIZE( ok ), nerr )
    endif
  end subroutine utest_assert_false_1
!
