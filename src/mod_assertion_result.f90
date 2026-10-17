module mod_assertion_result
  use, intrinsic :: iso_fortran_env, only: real64
  use mod_unittest_tap14_yaml, only: tap14_yaml_document_start, tap14_yaml_document_end, &
                                     tap14_yaml_body, tap14_yaml_list
  implicit none
  private

  integer, parameter, public :: ASSERTION_DETAIL_LEN = 512
  integer, parameter, public :: ASSERTION_METHOD_LEN = 64

  public :: n_test, n_error
  public :: report_rank_error, report_rank_error_tap

  type, public :: assertion_failure
    sequence
    integer :: flat_idx = 0
    character(ASSERTION_DETAIL_LEN) :: detail = ''
  end type assertion_failure

  type, public :: assertion_result
    integer :: id = 0
    integer, allocatable :: shape_a(:)
    integer, allocatable :: shape_b(:)
    character(ASSERTION_METHOD_LEN) :: method = ''
    character(ASSERTION_METHOD_LEN) :: unitname = ''
    ! Private vector storage; only elements 1:n_details are live.
    type(assertion_failure), allocatable, private :: details(:)
    integer, private :: n_details = 0
  contains
    procedure, public :: push => assertion_result_push
    procedure, public :: size => assertion_result_size
    procedure, public :: get_detail => assertion_result_get_detail
    procedure, public :: clear => assertion_result_clear
    procedure, public :: destroy => assertion_result_destroy
    procedure, public :: copy_from => assertion_result_copy_from
    procedure, public :: is_error => assertion_result_is_error
    procedure, public :: has_rank_error => assertion_result_has_rank_error
    procedure, public :: error_rate => assertion_result_error_rate
    procedure, public :: report_error_rate => assertion_result_report_error_rate
  end type assertion_result

contains

  pure function n_test(results) result(res)
    type(assertion_result), intent(in) :: results(:)
    integer :: res

    res = 0
    if (SIZE(results) > 0) res = results(SIZE(results))%id
  end function n_test

  pure function n_error(results) result(res)
    type(assertion_result), intent(in) :: results(:)
    integer :: res, first, i

    res = 0
    if (SIZE(results) == 0) return
    first = MAX(1, SIZE(results) - results(SIZE(results))%id + 1)
    do i = first, SIZE(results)
      if (results(i)%is_error()) res = res + 1
    end do
  end function n_error

  pure real(real64) function assertion_result_error_rate(this) result(rate)
    class(assertion_result), intent(in) :: this
    integer :: total_size

    if (ALLOCATED(this%shape_b)) then
      if (.not. ALLOCATED(this%shape_a)) then
        rate = 1.0_real64
        return
      end if
      if (SIZE(this%shape_a) /= SIZE(this%shape_b)) then
        rate = 1.0_real64
        return
      end if
      if (ANY(this%shape_a /= this%shape_b)) then
        rate = 1.0_real64
        return
      end if
    end if

    total_size = 1
    if (ALLOCATED(this%shape_a)) total_size = PRODUCT(this%shape_a)
    if (total_size == 0) then
      rate = 0.0_real64
    else
      rate = REAL(this%n_details, real64) / REAL(total_size, real64)
    end if
  end function assertion_result_error_rate

  subroutine assertion_result_report_error_rate(this, dev)
    class(assertion_result), intent(in) :: this
    integer, intent(in) :: dev
    integer :: total_size, ios
    real(real64) :: rate
    character(*), parameter :: spacer = '#'//REPEAT(' ', 7)
    character(*), parameter :: separator = spacer//REPEAT('-', 56)

    total_size = 1
    if (ALLOCATED(this%shape_a)) total_size = PRODUCT(this%shape_a)
    if (total_size <= 1) return
    rate = this%error_rate()
    write (dev, '(A)', iostat=ios) separator
    write (dev, '(A,F7.3,A,I0,A,I0,A)', iostat=ios) &
      spacer//'  ErrorRate: ', 100.0_real64 * rate, ' %  ( ', this%n_details, ' / ', total_size, ' )'
  end subroutine assertion_result_report_error_rate

  pure function report_rank_error(num_test, size_a, size_b, routine, unitname) result(message)
    integer, intent(in) :: num_test
    integer, intent(in) :: size_a(:), size_b(:)
    character(*), intent(in) :: routine
    character(*), intent(in), optional :: unitname
    character(:), allocatable :: message
    character(:), allocatable :: name
    character(7) :: count_text
    character(*), parameter :: spacer = '#'//REPEAT(' ', 7)
    character(*), parameter :: separator = spacer//REPEAT('-', 56)
    character(*), parameter :: failed = ACHAR(INT(z'1b'))//'[91mFAILED'//ACHAR(INT(z'1b'))//'[m'
    character(*), parameter :: rank_mismatch = spacer//'  RankMissMatch: '
    integer :: padding

    write (count_text, '(I7)') num_test
    name = result_name(unitname, routine, num_test)
    padding = MAX(0, 44 - LEN(name))
    message = '#'//count_text//' '//name//REPEAT(' ', padding)//'... '//failed//NEW_LINE('')// &
              separator//NEW_LINE('')//rank_mismatch//format_shape(size_a)//' /= '//format_shape(size_b)//NEW_LINE('')//separator
  end function report_rank_error

  pure function report_rank_error_tap(num_test, nest, size_a, size_b, routine, unitname) result(message)
    integer, intent(in) :: num_test, nest
    integer, intent(in) :: size_a(:), size_b(:)
    character(*), intent(in) :: routine
    character(*), intent(in), optional :: unitname
    character(:), allocatable :: message, name, assertion, status
    integer :: i

    status = tap_status(unitname, num_test)
    message = REPEAT('  ', MAX(0, nest))//'not '//status//NEW_LINE('')
    name = result_name(unitname, routine, num_test)
    assertion = routine
    if (assertion == '') assertion = 'assert'
    message = message//tap14_yaml_document_start(nest=nest + 1)
    message = message//tap14_yaml_body('name', '"'//name//'"', nest=nest + 1)
    message = message//tap14_yaml_body('assertion', assertion, nest=nest + 1)
    message = message//tap14_yaml_body('values', '', nest=nest + 1)
    message = message//tap14_yaml_body('rank', 'false', nest=nest + 1)
    message = message//tap14_yaml_body('shape_a', '', nest=nest + 1)
    do i = 1, SIZE(size_a)
      message = message//tap14_yaml_list(size_a(i), nest=nest + 2)
    end do
    message = message//tap14_yaml_body('shape_b', '', nest=nest + 2)
    do i = 1, SIZE(size_b)
      message = message//tap14_yaml_list(size_b(i), nest=nest + 2)
    end do
    message = message//tap14_yaml_document_end(nest=nest + 1)
  end function report_rank_error_tap

  pure function result_name(unitname, routine, num_test) result(name)
    character(*), intent(in), optional :: unitname
    character(*), intent(in) :: routine
    integer, intent(in) :: num_test
    character(:), allocatable :: name
    character(32) :: count_text

    if (PRESENT(unitname)) then
      name = TRIM(unitname)
    else
      write (count_text, '(I0)') num_test
      if (routine == '') then
        name = 'TEST '//TRIM(count_text)
      else
        name = routine//' '//TRIM(count_text)
      end if
    end if
  end function result_name

  pure function format_shape(shape) result(text)
    integer, intent(in) :: SHAPE(:)
    character(:), allocatable :: text
    character(32) :: extent_text
    integer :: i

    text = '['
    do i = 1, SIZE(shape)
      if (i > 1) text = text//', '
      write (extent_text, '(I0)') SHAPE(i)
      text = text//TRIM(extent_text)
    end do
    text = text//']'
  end function format_shape

  pure function tap_status(unitname, num_test) result(status)
    character(*), intent(in), optional :: unitname
    integer, intent(in) :: num_test
    character(:), allocatable :: status
    character(32) :: count_text

    write (count_text, '(I0)') num_test
    status = 'ok '//TRIM(count_text)
    if (PRESENT(unitname)) status = status//' # '//TRIM(unitname)
  end function tap_status

  pure subroutine assertion_result_push(this, flat_idx, detail)
    class(assertion_result), intent(inout) :: this
    integer, intent(in) :: flat_idx
    character(*), intent(in) :: detail
    type(assertion_failure), allocatable :: grown(:)
    integer :: capacity, new_capacity

    if (.not. ALLOCATED(this%details)) then
      allocate (this%details(1))
    else if (this%n_details == SIZE(this%details)) then
      capacity = SIZE(this%details)
      new_capacity = MAX(1, 2 * capacity)
      allocate (grown(new_capacity))
      grown(1:this%n_details) = this%details(1:this%n_details)
      call MOVE_ALLOC(grown, this%details)
    end if

    this%n_details = this%n_details + 1
    this%details(this%n_details)%flat_idx = flat_idx
    this%details(this%n_details)%detail = detail
  end subroutine assertion_result_push

  pure integer function assertion_result_size(this) result(n)
    class(assertion_result), intent(in) :: this
    n = this%n_details
  end function assertion_result_size

  pure logical function assertion_result_is_error(this) result(failed)
    class(assertion_result), intent(in) :: this

    failed = this%n_details > 0
    if (failed) return
    failed = this%has_rank_error()
  end function assertion_result_is_error

  pure logical function assertion_result_has_rank_error(this) result(failed)
    class(assertion_result), intent(in) :: this

    failed = .false.
    if (.not. ALLOCATED(this%shape_b)) return
    if (.not. ALLOCATED(this%shape_a)) then
      failed = .true.
      return
    end if
    if (SIZE(this%shape_a) /= SIZE(this%shape_b)) then
      failed = .true.
      return
    end if
    failed = ANY(this%shape_a /= this%shape_b)
  end function assertion_result_has_rank_error

  pure subroutine assertion_result_copy_from(this, source)
    class(assertion_result), intent(inout) :: this
    class(assertion_result), intent(in) :: source

    call this%clear()
    this%id = source%id
    this%method = source%method
    this%unitname = source%unitname
    if (ALLOCATED(source%shape_a)) this%shape_a = source%shape_a
    if (ALLOCATED(source%shape_b)) this%shape_b = source%shape_b
    this%n_details = source%n_details
    if (ALLOCATED(source%details)) this%details = source%details
  end subroutine assertion_result_copy_from

  pure function assertion_result_get_detail(this, idx) result(detail)
    class(assertion_result), intent(in) :: this
    integer, intent(in) :: idx
    type(assertion_failure) :: detail
    detail = this%details(idx)
  end function assertion_result_get_detail

  pure subroutine assertion_result_clear(this)
    class(assertion_result), intent(inout) :: this
    if (ALLOCATED(this%details)) deallocate (this%details)
    if (ALLOCATED(this%shape_a)) deallocate (this%shape_a)
    if (ALLOCATED(this%shape_b)) deallocate (this%shape_b)
    this%n_details = 0
    this%method = ''
    this%unitname = ''
    this%id = 0
  end subroutine assertion_result_clear

  pure subroutine assertion_result_destroy(this)
    class(assertion_result), intent(inout) :: this
    call this%clear()
  end subroutine assertion_result_destroy

end module mod_assertion_result
