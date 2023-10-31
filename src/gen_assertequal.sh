#!/bin/bash
gen_private_procedure(){
  post=("_00" "_01" "_10" "_11")
  for b in ${post[@]} ; do
    echo  '    procedure,private :: utest_'${1}${b}
  done
}

gen_generic_procedure(){
  post=("_00" "_01" "_10")
  for b in ${post[@]} ; do
    echo  '                       & utest_'${1}${b}', &'
  done
  echo -n '                       & utest_'${1}'_11'
}

gen_procedures(){
  for a in ${@:2:($#-1)} ; do
    gen_private_procedure $1_$a
  done
  echo '    generic           :: assert_'$1' =>      &'
  i=1
  for a in ${@:2:($#-1)} ; do
    gen_generic_procedure $1_$a
    ((i++))
    if [ $i -eq $# ] ; then
      break
    fi
    echo ', &'
  done
  echo ''
}

gen_head(){
    echo '  subroutine utest_'$1'( this, '$2', unitname )'
    if [ "$#" == "3" ]; then
      echo '  use,intrinsic :: ISO_FORTRAN_ENV, only : '$3
    fi
}

gen_foot(){
    echo '  end subroutine utest_'$1
    echo '!'
}

gen_vardef(){
    echo   '  class( unittest ),intent( inout ) :: this'
    echo   '  '$1',intent( in )      :: '$2
    echo   '  character( * ),intent( in )       :: unitname'
    echo   '  logical                           :: err'
    if [ "$#" == "3" ]; then
      echo '  integer( IK )                     :: i, ios, nerr'
    else
      echo '  integer( IK )                     :: ios'
    fi
}

gen_lankcheck(){
  echo   '    call utest_assert_lank_missmatch( this, SIZE( a ), SIZE( b ), unitname, err )'
  echo   '    if( err ) RETURN'
}

gen_proc(){

    echo   '    call utest_assert_printer( this, '$1', unitname, err )'
    if [ "$#" == "6" ]; then
      echo '    if( err )then'
      echo "      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError, "$2
      echo '      nerr = 0'
      echo '      do i=1,'$3
      echo '        if( '$4' ) CYCLE ; nerr = nerr + 1'
      echo "        write( STDOUT, '(6X,i8,A"$5")', IOSTAT=ios ) i, $6"
      echo '        if( ios>0 ) EXIT'
      echo '      enddo'
      echo '      call utest_error_rate_printer( '$3', nerr )'
      echo '    endif'
    else
      echo "    if( err ) write( STDOUT, '(2A"$2")', IOSTAT=ios ) AssertionError,"$3
    fi
}

gen_assert(){
  name=("" "_true" "_false")
  msg=("ASSERT" "ASSERT_TRUE" "ASSERT_FALSE")
  rou0=("ok" "ok" ".not.ok")
  rou1=("ALL( ok )" "ALL( ok )" ".not.ANY( ok )")
  for i in `seq 0 1 2` ; do
    gen_head   "assert${name[$i]}_0" "ok"
    gen_vardef "logical"  "ok"
    gen_proc   "${rou0[$i]}" "" "'  xxx ${msg[$i]} ERROR'"
    gen_foot   "assert${name[$i]}_0"

    gen_head   "assert${name[$i]}_1" "ok"
    gen_vardef "logical"  "ok(:)" 1
    gen_proc   "${rou1[$i]}" "'${msg[$i]} 1D'" "SIZE( ok )" ".not.ok(i)" "" "'  xxx error'"
    gen_foot   "assert${name[$i]}_1"
  done
}

gen_equal(){
  nm=(equal not_equal)
  eq=($4 $5)
  qe=($5 $4)
  for i in `seq 0 1` ; do
    AEQB="a${eq[$i]}b"
    AAEB="ALL( ${AEQB} )"
    A2="'LHS ${eq[$i]} RHS'"
    SA="SIZE( a )"
    SB="SIZE( b )"
    FMT=",$3,A,$3,A"

    gen_head "${nm[$i]}_$1_00" "a, b" $6
    gen_vardef "$2" "a, b"
    gen_proc "$AEQB" "$FMT" "'  xxx [', a, '] ${qe[$i]} [', b, ']'"
    gen_foot "${nm[$i]}_$1_00"

    gen_head "${nm[$i]}_$1_01" "a, b" $6
    gen_vardef "$2" "a, b(:)" 1
    gen_proc "$AAEB" "$A2" "$SB" "a${eq[$i]}b(i)" "$FMT" "'  xxx [', a, '] ${qe[$i]} [', b(i), ']'"
    gen_foot "${nm[$i]}_$1_01"

    gen_head "${nm[$i]}_$1_10" "a, b" $6
    gen_vardef "$2" "a(:), b" 1
    gen_proc "$AAEB" "$A2" "$SA" "a(i)${eq[$i]}b" "$FMT" "'  xxx [', a(i), '] ${qe[$i]} [', b, ']'"
    gen_foot "${nm[$i]}_$1_10"

    gen_head "${nm[$i]}_$1_11" "a, b" $6
    gen_vardef "$2" "a(:), b(:)" 1
    gen_lankcheck
    gen_proc "$AAEB" "$A2" "MINVAL( [ ${SA}, ${SB} ], 1 )" "a(i)${eq[$i]}b(i)" "$FMT" "'  xxx [', a(i), '] ${qe[$i]} [', b(i), ']'"
    gen_foot "${nm[$i]}_$1_11"

  done
  echo_bar1
}

gen_compare(){
  nm=(less less_equal greater greater_equal)
  eq=("< " "<=" "> " ">=")
  qe=(">=" "> " "<=" "< ")
  for i in `seq 0 3` ; do
    AEQB="a${eq[$i]}b"
    AAEB="ALL( ${AEQB} )"
    A2="'LHS ${eq[$i]} RHS'"
    SA="SIZE( a )"
    SB="SIZE( b )"
    FMT=",$3,A,$3,A"

    gen_head "${nm[$i]}_$1_00" "a, b" $4
    gen_vardef "$2" "a, b"
    gen_proc "$AEQB" "$FMT" "'  xxx [', a, '] ${qe[$i]} [', b, ']'"
    gen_foot "${nm[$i]}_$1_00"

    gen_head "${nm[$i]}_$1_01" "a, b" $4
    gen_vardef "$2" "a, b(:)" 1
    gen_proc "$AAEB" "$A2" "$SB" "a${eq[$i]}b(i)" "$FMT" "'  xxx [', a, '] ${qe[$i]} [', b(i), ']'"
    gen_foot "${nm[$i]}_$1_01"

    gen_head "${nm[$i]}_$1_10" "a, b" $4
    gen_vardef "$2" "a(:), b" 1
    gen_proc "$AAEB" "$A2" "$SA" "a(i)${eq[$i]}b" "$FMT" "'  xxx [', a(i), '] ${qe[$i]} [', b, ']'"
    gen_foot "${nm[$i]}_$1_10"

    gen_head "${nm[$i]}_$1_11" "a, b" $4
    gen_vardef "$2" "a(:), b(:)" 1
    gen_lankcheck
    gen_proc "$AAEB" "$A2" "MINVAL( [ ${SA}, ${SB} ], 1 )" "a(i)${eq[$i]}b(i)" "$FMT" "'  xxx [', a(i), '] ${qe[$i]} [', b(i), ']'"
    gen_foot "${nm[$i]}_$1_11"

  done
  echo_bar1
}

gen_head2(){
    echo '  subroutine utest_'$1'( this, a, b, unitname, place )'
    if [ "$#" == "2" ]; then
      echo '  use,intrinsic :: ISO_FORTRAN_ENV, only : '$2
    fi
}

gen_vardef2(){
    echo   '  class( unittest ),intent( inout )   :: this'
    echo   '  '$1',intent( in )        :: '$2
    echo   '  character( * ),intent( in )         :: unitname'
    echo   '  integer( IK ),intent( in ),optional :: place'
    echo   '  real( RK )                          :: mp'
    echo   '  logical                             :: err'
    if [ "$#" == "3" ]; then
      echo '  integer( IK ),allocatable           :: rud(:)'
      echo '  integer( IK )                       :: i, s, ios, nerr'
    else
      echo '  integer( IK )                       :: rud, ios'
    fi
}

gen_proc2(){
    echo   '    if( PRESENT( place ) )then ; mp = 1D1**place'
    echo   '    else                       ; mp = DEF_MP'
    echo   '    endif'
    if [ "$#" == "7" ]; then
      echo '    s = ' $5
      echo '    ALLOCATE( rud, source=[( '$1'_almost_equal( '$4', mp ), i=1,s )] )'
      echo '    call utest_assert_printer( this, '$2', unitname, err )'
      echo '    if( err )then'
      echo "      write( STDOUT, '(2A)', IOSTAT=ios ) AssertionError,'LHS "$6" RHS'"
      echo '      nerr = 0'
      echo '      do i=1,s'
      echo '        if( rud(i)<1 ) CYCLE ; nerr = nerr + 1'
      echo "        write( STDOUT, '(6X,i8,A,"$3",A,"$3",A)', IOSTAT=ios ) i, &"
      echo "       & "$7
      echo '        if( ios>0 ) EXIT'
      echo '      enddo'
      echo '      call utest_error_rate_printer( s, nerr )'
      echo '    endif'
      echo '    DEALLOCATE( rud )'
    else
      echo '    rud = '$1'_almost_equal( a, b, mp )'
      echo '    call utest_assert_printer( this, '$2', unitname, err )'
      echo "    if( err ) write( STDOUT,'(2A,"$3",A,"$3",A)', IOSTAT=ios ) AssertionError, '[',a,'] "$4" [',b,']'"
    fi
}

gen_almost_equal(){
  nm=(almost_equal not_almost_equal)
  eq=("< " ">=")
  qe=(">=" "< ")
  ne=("==" "/=")
  nn=("/=" "==")
  st=("xxx" "   ")
  ts=("   " "xxx")
  for i in `seq 0 1`
  do
    gen_head2   "${nm[$i]}_$1_00" $4
    gen_vardef2 "$2" "a, b"
    gen_proc2   "$1" "rud${eq[$i]}1" "$3" ${qe[$i]}
    gen_foot    "${nm[$i]}_$1_00"

    gen_head2   "${nm[$i]}_$1_01" $4
    gen_vardef2 "$2" "a, b(:)" 1
    gen_proc2   "$1" "ALL( rud${eq[$i]}1 )" "$3" "a, b(:)" "SIZE( b )" ${ne[$i]} "'  ${st[$i]} [', a, ']${nn[$i]}[', b(i), ']'"
    gen_foot    "${nm[$i]}_$1_01"

    gen_head2   "${nm[$i]}_$1_10" $4
    gen_vardef2 "$2" "a(:), b" 1
    gen_proc2   "$1" "ALL( rud${eq[$i]}1 )" "$3" "a(:), b" "SIZE( a )" ${ne[$i]} "'  ${st[$i]} [', a(i), ']${nn[$i]}[', b, ']'"
    gen_foot    "${nm[$i]}_$1_10"

    gen_head2   "${nm[$i]}_$1_11" $4
    gen_vardef2 "$2" "a(:), b(:)" 1
    gen_lankcheck
    gen_proc2   "$1" "ALL( rud${eq[$i]}1 )" "$3" "a(:), b(:)" "MINVAL( [ SIZE(a), SIZE(b) ], 1 )" ${ne[$i]} "'  ${st[$i]} [', a(i), ']${nn[$i]}[', b(i), ']'"
    gen_foot    "${nm[$i]}_$1_11"
  done
  echo_bar1

  echo   '  pure elemental function '${1}'_almost_equal( a, b, mp ) result( res )'
  if [ "$#" == "4" ]; then
    echo '  use,intrinsic :: ISO_FORTRAN_ENV, only : '${4}
  fi
  echo   '  '${2}',intent( in ) :: a,b'
  echo   '  real( RK ),intent( in )    :: mp'
  echo   '  real( RK )                 :: diff'
  echo   '  integer( IK )              :: res'
  echo   '    diff  = ABS( a - b )'
  echo   '    res   = INT( diff, IK )'
  echo   '    if( res>0 ) RETURN'
  echo   '    diff  = diff * mp'
  echo   '    res   = INT( diff, IK )'
  echo   '  end function '${1}'_almost_equal'
  echo   '!'
  echo_bar1
}

echo_bar1(){
  echo '!---------------------------------------------------------!'
  echo '!'
}
echo_bar(){
  echo '!=========================================================!'
  echo '!'
}

gen_assert_procedures(){
  name=("t" "t_true" "t_false")
  for n in ${name[@]} ; do
    echo '    procedure,private :: utest_asser'${n}_0
    echo '    procedure,private :: utest_asser'${n}_1
    echo '    generic           :: asser'${n}'  =>      &'
    echo '                       & utest_asser'${n}'_0, &'
    echo '                       & utest_asser'${n}'_1'
  done
}

gen_int_procedures(){
  name=("equal" "not_equal")
  for n in ${name[@]} ; do
    gen_procedures ${n} $@
  done
}

gen_float_procedures(){
  name=("less" "less_equal" "greater" "greater_equal" "almost_equal" "not_almost_equal")
  for n in ${name[@]} ; do
    gen_procedures ${n} $@
  done
}

gen_cmplx_procedures(){
  name=("almost_equal" "not_almost_equal")
  for n in ${name[@]} ; do
    gen_procedures ${n} $@
  done
}

gen_cord_equal(){
  gen_equal           byte 'integer( INT8 )'  I16 ==    /=     'INT8  '
  gen_equal           int2 'integer( INT16 )' I16 ==    /=     'INT16 '
  gen_equal           int4 'integer( INT32 )' I16 ==    /=     'INT32 '
  gen_equal           int8 'integer( INT64 )' I16 ==    /=     'INT64 '
  gen_equal           char 'character(*)'     A   ==    /=
  gen_equal           logi 'logical'          L16 .eqv. .neqv.
  echo_bar
}

gen_cord_compare(){
  gen_compare         byte 'integer( INT8 )'     I0    'INT8   '
  gen_compare         int2 'integer( INT16 )'    I0    'INT16  '
  gen_compare         int4 'integer( INT32 )'    I0    'INT32  '
  gen_compare         int8 'integer( INT64 )'    I0    'INT64  '
  gen_compare         real 'real( REAL32 )'      F16.9 'REAL32 '
  gen_compare         dble 'real( REAL64 )'      F16.9 'REAL64 '
  gen_compare         quad 'real( REAL128)'      F16.9 'REAL128'
  echo_bar
}

gen_cord_almost_equal(){
  gen_almost_equal    byte  'integer( INT8 )'    I0    'INT8   '
  gen_almost_equal    int2  'integer( INT16 )'   I0    'INT16  '
  gen_almost_equal    int4  'integer( INT32 )'   I0    'INT32  '
  gen_almost_equal    int8  'integer( INT64 )'   I0    'INT64  '
  gen_almost_equal    real  'real( REAL32 )'     F16.9 'REAL32 '
  gen_almost_equal    dble  'real( REAL64 )'     F16.9 'REAL64 '
  gen_almost_equal    quad  'real( REAL128)'     F16.9 'REAL128'
  gen_almost_equal    cmp4  'complex( REAL32 )' 2F16.9 'REAL32 '
  gen_almost_equal    cmp8  'complex( REAL64 )' 2F16.9 'REAL64 '
  gen_almost_equal    cmp16 'complex( REAL128)' 2F16.9 'REAL128'
  echo_bar
}

gen_assert_procedures                                   > unittest_assert.h
gen_int_procedures   byte int2 int4 int8 char logi      > unittest_assert_equal.h
gen_float_procedures byte int2 int4 int8 real dble quad > unittest_assert_compare.h
gen_cmplx_procedures cmp4 cmp8 cmp16                    > unittest_assert_almost_equal.h
gen_assert                                              > unittest_assert.f90
gen_cord_equal                                          > unittest_assert_equal.f90
gen_cord_compare                                        > unittest_assert_compare.f90
gen_cord_almost_equal                                   > unittest_assert_almost_equal.f90
