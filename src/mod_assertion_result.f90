module mod_assertion_result
  use, intrinsic :: iso_fortran_env, only: output_unit
  use mod_unittest_tap14_yaml, only: tap14_yaml_document_start, tap14_yaml_document_end, &
                                    tap14_yaml_body, tap14_yaml_list
  implicit none
  private

  integer, parameter, public :: ASSERTION_DETAIL_LEN = 512
  integer, parameter, public :: ASSERTION_METHOD_LEN = 64

  public :: test_result
  public :: n_test, n_error
  public :: report_rank_error, report_rank_error_tap

  type :: test_result
    sequence
    integer :: id
    logical :: err
  end type test_result

  type, public :: assertion_failure
    sequence
    integer :: flat_index = 0
    character(ASSERTION_DETAIL_LEN) :: detail = ''
  end type assertion_failure

  type, public :: assertion_result
    integer, allocatable :: shape_a(:)
    integer, allocatable :: shape_b(:)
    character(ASSERTION_METHOD_LEN) :: method = ''
    ! Private vector storage; only elements 1:n_details are live.
    type(assertion_failure), allocatable, private :: details(:)
    integer, private :: n_details = 0
  contains
    procedure, public :: push => assertion_result_push
    procedure, public :: size => assertion_result_size
    procedure, public :: get_detail => assertion_result_get_detail
    procedure, public :: clear => assertion_result_clear
    final :: assertion_result_finalize
  end type assertion_result

contains

  pure function n_test(results) result(res)
    type(test_result), intent(in) :: results(:)
    integer :: res

    res = 0
    if (size(results) > 0) res = results(size(results))%id
  end function n_test

  pure function n_error(results) result(res)
    type(test_result), intent(in) :: results(:)
    integer :: res, first

    res = 0
    if (size(results) == 0) return
    first = max(1, size(results) - results(size(results))%id + 1)
    res = count(results(first:)%err)
  end function n_error

  function report_rank_error(num_test, size_a, size_b, routine, unitname) result(message)
    integer, intent(in) :: num_test
    integer, intent(in) :: size_a(:), size_b(:)
    character(*), intent(in) :: routine
    character(*), intent(in), optional :: unitname
    character(:), allocatable :: message
    character(:), allocatable :: name
    character(7) :: count_text
    character(*), parameter :: spacer = '#'//repeat(' ', 7)
    character(*), parameter :: separator = spacer//repeat('-', 56)
    character(*), parameter :: failed = achar(int(z'1b'))//'[91mFAILED'//achar(int(z'1b'))//'[m'
    character(*), parameter :: rank_mismatch = spacer//'  RankMissMatch: '
    integer :: padding

    write (count_text, '(I7)') num_test
    name = result_name(unitname, routine, num_test)
    padding = max(0, 44 - len(name))
    message = '#'//count_text//' '//name//repeat(' ', padding)//'... '//failed//new_line('')// &
      separator//new_line('')//rank_mismatch//format_shape(size_a)//' /= '//format_shape(size_b)//new_line('')//separator
  end function report_rank_error

  subroutine report_rank_error_tap(num_test, nest, size_a, size_b, routine, unitname, output)
    integer, intent(in) :: num_test, nest
    integer, intent(in) :: size_a(:), size_b(:)
    character(*), intent(in) :: routine
    character(*), intent(in), optional :: unitname
    integer, intent(in), optional :: output
    character(:), allocatable :: name, assertion, status
    integer :: ios, i, output_unit_

    output_unit_ = output_unit
    if (present(output)) output_unit_ = output
    status = tap_status(unitname, num_test)
    write (output_unit_, '(A)', iostat=ios) repeat('  ', max(0, nest))//'not '//status
    name = result_name(unitname, routine, num_test)
    assertion = routine
    if (assertion == '') assertion = 'assert'
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_document_start(nest=nest + 1)
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_body('name', '"'//name//'"', nest=nest + 1)
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_body('assertion', assertion, nest=nest + 1)
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_body('values', '', nest=nest + 1)
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_body('rank', 'false', nest=nest + 1)
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_body('shape_a', '', nest=nest + 1)
    do i = 1, size(size_a)
      write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_list(size_a(i), nest=nest + 2)
    end do
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_body('shape_b', '', nest=nest + 2)
    do i = 1, size(size_b)
      write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_list(size_b(i), nest=nest + 2)
    end do
    write (output_unit_, '(A)', advance='NO', iostat=ios) tap14_yaml_document_end(nest=nest + 1)
  end subroutine report_rank_error_tap

  pure function result_name(unitname, routine, num_test) result(name)
    character(*), intent(in), optional :: unitname
    character(*), intent(in) :: routine
    integer, intent(in) :: num_test
    character(:), allocatable :: name
    character(32) :: count_text

    if (present(unitname)) then
      name = trim(unitname)
    else
      write (count_text, '(I0)') num_test
      if (routine == '') then
        name = 'TEST '//trim(count_text)
      else
        name = routine//' '//trim(count_text)
      end if
    end if
  end function result_name

  function format_shape(shape) result(text)
    integer, intent(in) :: shape(:)
    character(:), allocatable :: text
    character(32) :: extent_text
    integer :: i

    text = '['
    do i = 1, size(shape)
      if (i > 1) text = text//', '
      write (extent_text, '(I0)') shape(i)
      text = text//trim(extent_text)
    end do
    text = text//']'
  end function format_shape

  pure function tap_status(unitname, num_test) result(status)
    character(*), intent(in), optional :: unitname
    integer, intent(in) :: num_test
    character(:), allocatable :: status
    character(32) :: count_text

    write (count_text, '(I0)') num_test
    status = 'ok '//trim(count_text)
    if (present(unitname)) status = status//' # '//trim(unitname)
  end function tap_status

  pure subroutine assertion_result_push(this, flat_index, detail)
    class(assertion_result), intent(inout) :: this
    integer, intent(in) :: flat_index
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
    this%details(this%n_details)%flat_index = flat_index
    this%details(this%n_details)%detail = detail
  end subroutine assertion_result_push

  pure integer function assertion_result_size(this) result(n)
    class(assertion_result), intent(in) :: this
    n = this%n_details
  end function assertion_result_size

  pure function assertion_result_get_detail(this, index) result(detail)
    class(assertion_result), intent(in) :: this
    integer, intent(in) :: index
    type(assertion_failure) :: detail
    detail = this%details(index)
  end function assertion_result_get_detail

  pure subroutine assertion_result_clear(this)
    class(assertion_result), intent(inout) :: this
    if (ALLOCATED(this%details)) deallocate (this%details)
    if (ALLOCATED(this%shape_a)) deallocate (this%shape_a)
    if (ALLOCATED(this%shape_b)) deallocate (this%shape_b)
    this%n_details = 0
    this%method = ''
  end subroutine assertion_result_clear

  pure subroutine assertion_result_finalize(this)
    type(assertion_result), intent(inout) :: this
    call this%clear()
  end subroutine assertion_result_finalize

end module mod_assertion_result
