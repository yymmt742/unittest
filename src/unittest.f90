module mod_unittest
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT, &
                &                        IK     => INT32,    &
                &                        RK     => REAL64
implicit none
private
public  :: unittest
!
  character( 60 ),parameter :: SEP1   = REPEAT( '-', 60 )
  character( 60 ),parameter :: SEP2   = REPEAT( '=', 60 )
  character( 42 ),parameter :: SEP3   = REPEAT( ' ', 8 )//REPEAT( '-', 36 )
  character( 26 ),parameter :: AssertionError     = '        Assertion Error : '
  character( 26 ),parameter :: LankMissMatchError = '   Lank MissMatch Error : '
  character( 26 ),parameter :: ErrorRateIs        = '          Error rate is : '

  integer( IK ),parameter   :: DEF_MP = 10**4
!
  type unittest
    integer( IK )              :: num_test
    integer( IK )              :: num_error
    integer( IK )              :: start_time
    real( RK )                 :: start_cpu_time
    character( : ),allocatable :: script_name
    logical                    :: error_detected = .FALSE.
  contains
    procedure         :: init                 => utest_init
    include "unittest_assert.h"
    include "unittest_assert_equal.h"
    include "unittest_assert_compare.h"
    include "unittest_assert_almost_equal.h"
    procedure         :: finish               => utest_finish
    procedure         :: finish_and_terminate => utest_finish_and_terminate
    final             :: utest_destroy
  end type unittest
!
contains
!
include "unittest_assert.f90"
include "unittest_assert_equal.f90"
include "unittest_assert_compare.f90"
include "unittest_assert_almost_equal.f90"
!
!=========================================================!
!
  subroutine utest_init(this,section)
  class( unittest ),intent( inout )    :: this
  character( * ),intent( in ),optional :: section
  integer( IK )                        :: lna,ios
  character( : ),allocatable           :: tmp
    call utest_destroy(this)
!
    call cpu_time(this%start_cpu_time)
    call system_clock(this%start_time)
!
    call GET_COMMAND_ARGUMENT(0,length=lna,status=ios)
    if(ios/=0) RETURN
!
    allocate(character(lna) :: tmp)
    allocate(character(0) :: this%script_name)
    call GET_COMMAND_ARGUMENT(0,value=tmp,status=ios)
    lna = index(tmp,'/',.TRUE.) + 1
    this%script_name = tmp(lna:)
    deallocate(tmp)
!
    write( STDOUT, '(A)',err=100) SEP2
    write( STDOUT, '(A)',err=100) 'Test command :: '//this%script_name
    if(present(section)) write( STDOUT, '(A)',err=100) '  Section ----> '//section
!
100 RETURN
  end subroutine utest_init
!
  subroutine utest_assert_lank_missmatch( this, size_a, size_b, unitname, err )
  class( unittest ),intent( inout ) :: this
  integer( IK ),intent( in )        :: size_a, size_b
  character( * ),intent( in )       :: unitname
  logical,intent( inout )           :: err
  integer( IK )                     :: ios
!
    err = size_a/=size_b
!
    if( .not.ALLOCATED( this%script_name ) ) RETURN
    if( .not.err ) RETURN
    this%num_test  = this%num_test  + 1
    this%num_error = this%num_error + 1
!
    write( STDOUT, '(I8,A)', IOSTAT=ios ) this%num_test,' '//unitname//' ... failed ( lank miss match )'
    write( STDOUT,'(2A,I0,A,I0,A)', IOSTAT=ios ) LankMissMatchError, '[',size_a,'] /= [',size_b,']'
    FLUSH( STDOUT )
!
  end subroutine utest_assert_lank_missmatch
!
  subroutine utest_assert_printer( this, ok, unitname, err )
  class( unittest ),intent( inout ) :: this
  logical,intent( in )              :: ok
  character( * ),intent( in )       :: unitname
  logical,intent( inout )           :: err
  integer( IK )                     :: ios
    err = .FALSE.
    if(.not.ALLOCATED(this%script_name)) RETURN
    err = .not.ok
!
    this%num_test  = this%num_test  + 1
!
    write( STDOUT, '(I8,A)', ADVANCE='NO', IOSTAT=ios ) this%num_test,' '//unitname//' ... '
!
    if(ok)then
      write( STDOUT, '(A)', IOSTAT=ios ) 'ok'
    else
      write( STDOUT, '(A)', IOSTAT=ios ) 'failed'
      this%num_error = this%num_error + 1
    endif
!
    FLUSH( STDOUT )
  end subroutine utest_assert_printer
!
  subroutine utest_error_rate_printer( ntest, nerror )
  integer( IK ),intent( in )        :: ntest, nerror
  real( RK )                        :: error_rate
  integer( IK )                     :: ios
    if( ntest<1 ) RETURN
    error_rate = REAL( nerror, RK ) / REAL( ntest, RK )
    write( STDOUT, '(A)',        IOSTAT=ios ) SEP3
    write( STDOUT, '(A,f7.3,A)', IOSTAT=ios ) ErrorRateIs, error_rate, ' %'
    FLUSH( STDOUT )
  end subroutine utest_error_rate_printer
!
! subroutine utest_assert_0( this, ok, unitname )
! class( unittest ),intent( inout ) :: this
! logical,intent( in )              :: ok
! character( * ),intent( in )       :: unitname
! logical                           :: err
!   call utest_assert_printer( this, ok, unitname, err )
! end subroutine utest_assert_0
!
! subroutine utest_assert_1( this, ok, unitname )
! class( unittest ),intent( inout ) :: this
! logical,intent( in )              :: ok(:)
! character( * ),intent( in )       :: unitname
! logical                           :: err
!   call utest_assert_printer( this, ALL( ok ), unitname, err )
!   if(err)then
!     block
!     real( RK )   :: error_rate
!     integer( IK ) :: i
!     write( STDOUT, '(A)', err=100 ) AssertionError
!     error_rate = 0D0
!     do i=1,size(ok)
!       if( .not.ok(i) )then
!         write( STDOUT, '(6X,i8)',err=100) i
!         error_rate = error_rate + 1d2
!       endif
!     enddo
!     error_rate = error_rate / dble(size(ok))
!     write( STDOUT, '(A)',err=100) '        ----------------------------------'
!     write( STDOUT, '(A,f7.3,A)',err=100) '         Error rate is :',error_rate,' %'
!     end block
!   endif
!00 FLUSH( STDOUT )
!   RETURN
! end subroutine utest_assert_1
!
! subroutine utest_assert_false_0( this, ng, unitname )
! class( unittest ),intent( inout ) :: this
! logical,intent( in )              :: ng
! character( * ),intent( in )       :: unitname
! logical                           :: err
!   call utest_assert_printer(this,.not.ng,unitname,err)
! end subroutine utest_assert_false_0
!
! subroutine utest_assert_false_1( this, ok, unitname )
! class( unittest ),intent( inout ) :: this
! logical,intent( in )              :: ok(:)
! character( * ),intent( in )       :: unitname
! logical                           :: err
!   call utest_assert_printer( this, .not.ANY( ok ), unitname, err )
!   if(err)then
!     block
!     real( RK )    :: error_rate
!     integer( IK ) :: i
!     write( STDOUT, '(A)', err=100 ) '        AssertionError'
!     error_rate = 0D0
!     do i=1,size(ok)
!       if(ok(i))then
!         write( STDOUT, '(6X,i8)',err=100) i
!         error_rate = error_rate + 1d2
!       endif
!     enddo
!     error_rate = error_rate / dble(size(ok))
!     write( STDOUT, '(A)',err=100) '        ----------------------------------'
!     write( STDOUT, '(A,f7.3,A)',err=100) '         Error rate is :',error_rate,' %'
!     end block
!   endif
!00 FLUSH( STDOUT )
!   RETURN
! end subroutine utest_assert_false_1
!
  subroutine utest_finish( this )
  class( unittest ),intent( inout ) :: this
  integer( IK )                     :: finish_time,t_rate,t_max
  real( RK )                        :: finish_cpu_time,time
!
    if( .not.ALLOCATED( this%script_name ) ) RETURN
!
    call SYSTEM_CLOCK( finish_time, t_rate, t_max )
!
    if( finish_time<this%start_time )then
      time = ( t_max-this%start_time + finish_time + 1 )
    else
      time = ( finish_time-this%start_time )
    endif
!
    call CPU_TIME( finish_cpu_time )
!
    finish_cpu_time = finish_cpu_time - this%start_cpu_time
!
    if( this%num_test>0 )then
      write( STDOUT, '(A)', err=100 ) SEP1
      write( STDOUT, '(A,I0,A,F9.3,A,F9.3,A)', advance='NO', err=100 )    &
   &  ' Run ',this%num_test,' tests in ',time/dble(t_rate),' / ', &
   &  finish_cpu_time,' s (sys/cpu)----> '
      if( this%num_error<1 )then
        write( STDOUT, '(A,I0,A,F9.3,A)',err=100) 'Passed'
      else
        this%error_detected = .TRUE.
        write( STDOUT, '(A,/,I12,A)',err=100) 'Failed, ',this%num_error,' error detected.'
      endif
    else
      write( STDOUT, '(A)',err=100 ) 'No Test executed'
    endif
    write( STDOUT, '(A)', err=100 ) SEP2
!
100 FLUSH( STDOUT )
    call utest_free( this )
  end subroutine utest_finish
!
  subroutine utest_finish_and_terminate(this)
  class( unittest ),intent( inout ) :: this
    call utest_destroy( this )
    if( this%error_detected ) ERROR STOP 'TESTS WERE TERMINATED'
  end subroutine utest_finish_and_terminate
!
  pure subroutine utest_free( this )
  type( unittest ),intent( inout ) :: this
    if( ALLOCATED( this%script_name ) ) DEALLOCATE( this%script_name )
    this%num_test  = 0
    this%num_error = 0
  end subroutine utest_free
!
  subroutine utest_destroy(this)
  type( unittest ),intent( inout ) :: this
    if( ALLOCATED( this%script_name ) ) call this%finish()
    call utest_free( this )
  end subroutine utest_destroy
!
end module mod_unittest
