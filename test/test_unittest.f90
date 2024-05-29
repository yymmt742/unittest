program main
  use, intrinsic :: ISO_FORTRAN_ENV, only: INT8, INT16, INT32, INT64, &
    &                                      REAL32, REAL64, REAL128, ERROR_UNIT
  use mod_unittest
  implicit none
  type(unittest)              :: u
  logical, parameter          :: T = .true., F = .false.
  logical, parameter          :: T1(3) = [T, T, T], F1(3) = [F, F, F]
  integer(INT8), parameter    :: i1 = 1, j1(3) = [1, 1, 1], k1(3) = [2, 3, 4]
  integer(INT16), parameter   :: i2 = 1, j2(3) = [1, 1, 1], k2(3) = [2, 3, 4]
  integer(INT32), parameter   :: i4 = 1, j4(3) = [1, 1, 1], k4(3) = [2, 3, 4]
  integer(INT64), parameter   :: i8 = 1, j8(3) = [1, 1, 1], k8(3) = [2, 3, 4]
  real(REAL32), parameter     :: a4 = 1, b4(3) = [1, 1, 1], c4(3) = [2, 3, 4]
  real(REAL64), parameter     :: a8 = 1, b8(3) = [1, 1, 1], c8(3) = [2, 3, 4]
  complex(REAL32), parameter  :: x4 = 1, y4(3) = [1, 1, 1], z4(3) = [2, 3, 4]
  complex(REAL64), parameter  :: x8 = 1, y8(3) = [1, 1, 1], z8(3) = [2, 3, 4]
  real(REAL64)                :: rn(50, 50)
  complex(REAL64)             :: xe(50, 50)
  integer                     :: i, j
!
  call u%init('test_unittest', terminate_with_error_code=.false.)
!<&
  call u%assert(T,                         'assert               bool    0  ')
  call u%assert(T1,                        'assert               bool    1  ')
  call u%assert_true(T,                    'assert_true          bool    0  ')
  call u%assert_true(T1,                   'assert_true          bool    1  ')
  call u%assert_false(F,                   'assert_false         bool    0  ')
  call u%assert_false(F1,                  'assert_false         bool    1  ')
  call u%assert_equal(T, T,                'assert_equal         bool    00 ')
  call u%assert_equal(T1, T,               'assert_equal         bool    10 ')
  call u%assert_equal(F1, F,               'assert_equal         bool    10 ')
  call u%assert_equal(T1, T1,              'assert_equal         bool    11 ')
  call u%assert_equal(F1, F1,              'assert_equal         bool    11 ')
!
  call u%assert_equal(i1, i1,              'assert_equal         int8    00 ')
  call u%assert_equal(j1, i1,              'assert_equal         int8    10 ')
  call u%assert_equal(k1, k1,              'assert_equal         int8    11 ')
  call u%assert_not_equal(i1, k1(1),       'assert_not_equal     int8    00 ')
  call u%assert_not_equal(k1, i1,          'assert_not_equal     int8    10 ')
  call u%assert_not_equal(j1, k1,          'assert_not_equal     int8    11 ')
  call u%assert_less(i1, k1(1),            'assert_less          int8    00 ')
  call u%assert_less(j1, k1(1),            'assert_less          int8    10 ')
  call u%assert_less(j1, k1,               'assert_less          int8    11 ')
  call u%assert_greater(k1, i1,            'assert_greater       int8    10 ')
  call u%assert_greater(k1, j1,            'assert_greater       int8    11 ')
  call u%assert_less_equal(i1, i1,         'assert_less_equal    int8    00 ')
  call u%assert_less_equal(j1, i1,         'assert_less_equal    int8    10 ')
  call u%assert_less_equal(j1, k1 - i1,    'assert_less_equal    int8    11 ')
  call u%assert_greater_equal(i1, i1,      'assert_greater_equal int8    00 ')
  call u%assert_greater_equal(k1 - i1, i1, 'assert_greater_equal int8    10 ')
  call u%assert_greater_equal(k1, j1,      'assert_greater_equal int8    11 ')
!
  call u%assert_equal(i2, i2,              'assert_equal         int16   00 ')
  call u%assert_equal(j2, i2,              'assert_equal         int16   10 ')
  call u%assert_equal(k2, k2,              'assert_equal         int16   11 ')
  call u%assert_not_equal(i2, k2(1),       'assert_not_equal     int16   00 ')
  call u%assert_not_equal(k2, i2,          'assert_not_equal     int16   10 ')
  call u%assert_not_equal(j2, k2,          'assert_not_equal     int16   11 ')
  call u%assert_less(i2, k2(1),            'assert_less          int16   00 ')
  call u%assert_less(j2, k2(1),            'assert_less          int16   10 ')
  call u%assert_less(j2, k2,               'assert_less          int16   11 ')
  call u%assert_greater(k2(1), i2,         'assert_greater       int16   00 ')
  call u%assert_greater(k2, i2,            'assert_greater       int16   10 ')
  call u%assert_greater(k2, j2,            'assert_greater       int16   11 ')
  call u%assert_less_equal(i2, i2,         'assert_less_equal    int16   00 ')
  call u%assert_less_equal(j2, i2,         'assert_less_equal    int16   10 ')
  call u%assert_less_equal(j2, k2 - i2,    'assert_less_equal    int16   11 ')
  call u%assert_greater_equal(i2, i2,      'assert_greater_equal int16   00 ')
  call u%assert_greater_equal(k2 - i2, i2, 'assert_greater_equal int16   10 ')
  call u%assert_greater_equal(k2, j2,      'assert_greater_equal int16   11 ')
!
  call u%assert_equal(i4, i4,              'assert_equal         int32   00 ')
  call u%assert_equal(j4, i4,              'assert_equal         int32   10 ')
  call u%assert_equal(k4, k4,              'assert_equal         int32   11 ')
  call u%assert_not_equal(i4, k4(1),       'assert_not_equal     int32   00 ')
  call u%assert_not_equal(k4, i4,          'assert_not_equal     int32   10 ')
  call u%assert_not_equal(j4, k4,          'assert_not_equal     int32   11 ')
  call u%assert_less(i4, k4(1),            'assert_less          int32   00 ')
  call u%assert_less(j4, k4(1),            'assert_less          int32   10 ')
  call u%assert_less(j4, k4,               'assert_less          int32   11 ')
  call u%assert_greater(k4(1), i4,         'assert_greater       int32   00 ')
  call u%assert_greater(k4, i4,            'assert_greater       int32   10 ')
  call u%assert_greater(k4, j4,            'assert_greater       int32   11 ')
  call u%assert_less_equal(i4, i4,         'assert_less_equal    int32   00 ')
  call u%assert_less_equal(j4, i4,         'assert_less_equal    int32   10 ')
  call u%assert_less_equal(j4, k4 - i4,    'assert_less_equal    int32   11 ')
  call u%assert_greater_equal(i4, i4,      'assert_greater_equal int32   00 ')
  call u%assert_greater_equal(k4 - i4, i4, 'assert_greater_equal int32   10 ')
  call u%assert_greater_equal(k4, j4,      'assert_greater_equal int32   11 ')
!
  call u%assert_equal(i8, i8,              'assert_equal         int64   00 ')
  call u%assert_equal(j8, i8,              'assert_equal         int64   10 ')
  call u%assert_equal(k8, k8,              'assert_equal         int64   11 ')
  call u%assert_not_equal(i8, k8(1),       'assert_not_equal     int64   00 ')
  call u%assert_not_equal(k8, i8,          'assert_not_equal     int64   10 ')
  call u%assert_not_equal(j8, k8,          'assert_not_equal     int64   11 ')
  call u%assert_less(i8, k8(1),            'assert_less          int64   00 ')
  call u%assert_less(j8, k8(1),            'assert_less          int64   10 ')
  call u%assert_less(j8, k8,               'assert_less          int64   11 ')
  call u%assert_greater(k8(1), i8,         'assert_greater       int64   00 ')
  call u%assert_greater(k8, i8,            'assert_greater       int64   10 ')
  call u%assert_greater(k8, j8,            'assert_greater       int64   11 ')
  call u%assert_less_equal(i8, i8,         'assert_less_equal    int64   00 ')
  call u%assert_less_equal(j8, i8,         'assert_less_equal    int64   10 ')
  call u%assert_less_equal(j8, k8 - i8,    'assert_less_equal    int64   11 ')
  call u%assert_greater_equal(i8, i8,      'assert_greater_equal int64   00 ')
  call u%assert_greater_equal(k8 - i8, i8, 'assert_greater_equal int64   10 ')
  call u%assert_greater_equal(k8, j8,      'assert_greater_equal int64   11 ')
!
  call u%assert_almost_equal(a4, a4,       'assert_almost_equal  real32  00 ')
  call u%assert_almost_equal(b4, a4,       'assert_almost_equal  real32  10 ')
  call u%assert_almost_equal(c4, c4,       'assert_almost_equal  real32  11 ')
  call u%assert_less(a4, c4(1),            'assert_less          real32  00 ')
  call u%assert_less(b4, c4(1),            'assert_less          real32  10 ')
  call u%assert_less(b4, c4,               'assert_less          real32  11 ')
  call u%assert_greater(c4(1), a4,         'assert_greater       real32  00 ')
  call u%assert_greater(c4, a4,            'assert_greater       real32  10 ')
  call u%assert_greater(c4, b4,            'assert_greater       real32  11 ')
  call u%assert_less_equal(a4, a4,         'assert_less_equal    real32  00 ')
  call u%assert_less_equal(b4, a4,         'assert_less_equal    real32  10 ')
  call u%assert_less_equal(b4, c4 - a4,    'assert_less_equal    real32  11 ')
  call u%assert_greater_equal(a4, a4,      'assert_greater_equal real32  00 ')
  call u%assert_greater_equal(c4 - a4, a4, 'assert_greater_equal real32  10 ')
  call u%assert_greater_equal(c4, b4,      'assert_greater_equal real32  11 ')
!
  call u%assert_almost_equal(a8, a8,       'assert_almost_equal  real64  00 ')
  call u%assert_almost_equal(b8, a8,       'assert_almost_equal  real64  10 ')
  call u%assert_almost_equal(c8, c8,       'assert_almost_equal  real64  11 ')
  call u%assert_less(a8, c8(1),            'assert_less          real64  00 ')
  call u%assert_less(b8, c8(1),            'assert_less          real64  10 ')
  call u%assert_less(b8, c8,               'assert_less          real64  11 ')
  call u%assert_greater(c8(1), a8,         'assert_greater       real64  00 ')
  call u%assert_greater(c8, a8,            'assert_greater       real64  10 ')
  call u%assert_greater(c8, b8,            'assert_greater       real64  11 ')
  call u%assert_less_equal(a8, a8,         'assert_less_equal    real64  00 ')
  call u%assert_less_equal(b8, a8,         'assert_less_equal    real64  10 ')
  call u%assert_less_equal(b8, c8 - a8,    'assert_less_equal    real64  11 ')
  call u%assert_greater_equal(a8, a8,      'assert_greater_equal real64  00 ')
  call u%assert_greater_equal(c8 - a8, a8, 'assert_greater_equal real64  10 ')
  call u%assert_greater_equal(c8, b8,      'assert_greater_equal real64  11 ')
!
  call u%assert_almost_equal(x4, x4,       'assert_almost_equal  cmpx32  00 ')
  call u%assert_almost_equal(y4, x4,       'assert_almost_equal  cmpx32  10 ')
  call u%assert_almost_equal(z4, z4,       'assert_almost_equal  cmpx32  11 ')
  call u%assert_almost_equal(z4, -z4,      'assert_almost_equal  cmpx32  11 ')
!
  call u%assert_almost_equal(x8, x8,       'assert_almost_equal  cmpx64  00 ')
  call u%assert_almost_equal(y8, x8,       'assert_almost_equal  cmpx64  10 ')
  call u%assert_almost_equal(z8, z8,       'assert_almost_equal  cmpx64  11 ')
!
  call u%assert_almost_equal([-z8, z8], [z8,-z8],      'assert_almost_equal  cmpx64  11 ')
  call u%assert_almost_equal([z8, z8], -z8, 'assert_almost_equal  cmpx64  11 ')
  do concurrent(i=1:SIZE(xe, 1), j=1:SIZE(xe, 2))
    xe(i, j) = MERGE(1, 0, i==j)
  enddo
  call u%assert_is_eye(xe,                 'assert_is_eye        cmpx64')
  stop
  call random_number(rn)
  xe = rn - 0.5
  call random_number(rn)
  xe = xe + CMPLX(0.0, rn - 0.5)
  call u%assert_is_eye(xe,             'assert_is_eye        cmpx64')
  call u%assert_is_zero(xe,            'assert_is_zero       cmpx64')
  call u%assert_is_zero(xe,                'assert_is_zero       cmpx64')
  call u%assert_is_zero(xe,                'assert_is_zero       cmpx64')
  xe = 0.0
  call u%assert_is_zero(xe,                'assert_is_zero       cmpx64')
!&>
  call u%init('test_unittest', terminate_with_error_code=.false.)
  call u%assert(T,                         'assert               bool    0  ')
  call u%assert_almost_equal(x4, x4,       'assert_almost_equal  cmpx32  00 ')
  do concurrent(i=1:SIZE(xe, 1), j=1:SIZE(xe, 2))
    xe(i, j) = MERGE(1, 0, i==j)
  enddo
  call u%assert_is_eye(xe,                 'assert_is_eye        cmpx128')
!
  call u%finish_and_terminate()
!
end program main
