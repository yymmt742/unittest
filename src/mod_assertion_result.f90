module mod_assertion_result
  use, intrinsic :: iso_fortran_env, only: real64
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
    integer :: flat_idx = 0
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
    procedure, public :: destroy => assertion_result_destroy
    procedure, public :: error_rate => assertion_result_error_rate
    procedure, public :: report_error_rate => assertion_result_report_error_rate
    procedure, public :: report_result => assertion_result_report_result
  end type assertion_result

contains

  pure function n_test(results) result(res)
    type(test_result), intent(in) :: results(:)
    integer :: res

    res = 0
    if (SIZE(results) > 0) res = results(SIZE(results))%id
  end function n_test

  pure function n_error(results) result(res)
    type(test_result), intent(in) :: results(:)
    integer :: res, first

    res = 0
    if (SIZE(results) == 0) return
    first = MAX(1, SIZE(results) - results(SIZE(results))%id + 1)
    res = COUNT(results(first:)%err)
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

  subroutine assertion_result_report_result(this, dev, num_test, nest, tap, unitname, show_details)
    class(assertion_result), intent(in) :: this
    integer, intent(in) :: dev, num_test, nest
    logical, intent(in) :: tap
    character(*), intent(in), optional :: unitname
    logical, intent(in), optional :: show_details
    character(:), allocatable :: name, message, prefix, status
    character(*), parameter :: spacer = '#'//REPEAT(' ', 7)
    character(*), parameter :: failed = ACHAR(INT(z'1b'))//'[91mFAILED'//ACHAR(INT(z'1b'))//'[m'
    character(*), parameter :: passed = ACHAR(INT(z'1b'))//'[92mOK'//ACHAR(INT(z'1b'))//'[m'
    integer :: i, ios
    logical :: show, rank_error

    name = result_name(unitname, TRIM(this%method), num_test)
    show = .true.
    if (PRESENT(show_details)) show = show_details
    rank_error = .false.
    if (ALLOCATED(this%shape_b)) then
      rank_error = .not. ALLOCATED(this%shape_a)
      if (ALLOCATED(this%shape_a)) then
        rank_error = SIZE(this%shape_a) /= SIZE(this%shape_b)
        if (.not. rank_error) rank_error = ANY(this%shape_a /= this%shape_b)
      end if
    end if
    if (tap) then
      status = tap_status(unitname, num_test)
      if (this%n_details > 0 .or. rank_error) status = 'not '//status
      write (dev, '(A)', iostat=ios) REPEAT('  ', MAX(0, nest))//status
      if (this%n_details == 0 .and. .not. rank_error) return
      message = ''
      message = message//tap14_yaml_document_start(nest=nest + 1)
      message = message//tap14_yaml_body('name', '"'//name//'"', nest=nest + 1)
      message = message//tap14_yaml_body('assertion', TRIM(this%method), nest=nest + 1)
      message = message//tap14_yaml_body('values', '', nest=nest + 1)
      if (rank_error) then
        message = message//tap14_yaml_body('rank', 'false', nest=nest + 1)
        if (ALLOCATED(this%shape_a)) then
          message = message//tap14_yaml_body('shape_a', '', nest=nest + 1)
          do i = 1, SIZE(this%shape_a)
            message = message//tap14_yaml_list(this%shape_a(i), nest=nest + 2)
          end do
        end if
        message = message//tap14_yaml_body('shape_b', '', nest=nest + 2)
        do i = 1, SIZE(this%shape_b)
          message = message//tap14_yaml_list(this%shape_b(i), nest=nest + 2)
        end do
      end if
      if (this%n_details > 0) then
        message = message//tap14_yaml_body('indices', '', nest=nest + 1)
        do i = 1, this%n_details
          message = message//tap14_yaml_list(this%details(i)%flat_idx, nest=nest + 2)
        end do
        message = message//tap14_yaml_body('details', '', nest=nest + 1)
        do i = 1, this%n_details
          message = message//tap14_yaml_list('"'//TRIM(this%details(i)%detail)//'"', nest=nest + 2)
        end do
      end if
      message = message//tap14_yaml_document_end(nest=nest + 1)
      write (dev, '(A)', iostat=ios) message
      return
    end if

    call format_result_prefix(unitname, num_test, TRIM(this%method), prefix)
    if (this%n_details > 0 .or. rank_error) then
      write (dev, '(A)', iostat=ios) prefix//failed
      if (show) then
        do i = 1, this%n_details
          write (dev, '(A,I0,1X,A)', iostat=ios) spacer//'  ', this%details(i)%flat_idx, &
            TRIM(this%details(i)%detail)
        end do
      end if
      if (rank_error .and. ALLOCATED(this%shape_a)) then
        write (dev, '(A)', iostat=ios) spacer//'  RankMissMatch: '// &
          format_shape(this%shape_a)//' /= '//format_shape(this%shape_b)
      end if
      call this%report_error_rate(dev)
    else
      write (dev, '(A)', iostat=ios) prefix//passed
    end if
  end subroutine assertion_result_report_result

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

  pure subroutine format_result_prefix(unitname, num_test, routine, prefix)
    character(*), intent(in), optional :: unitname
    integer, intent(in) :: num_test
    character(*), intent(in) :: routine
    character(:), allocatable, intent(out) :: prefix
    character(:), allocatable :: name
    character(7) :: count_text
    integer :: padding

    write (count_text, '(I7)') num_test
    name = result_name(unitname, routine, num_test)
    padding = MAX(0, 44 - LEN(name))
    prefix = '#'//count_text//' '//name//REPEAT(' ', padding)//'... '
  end subroutine format_result_prefix

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
  end subroutine assertion_result_clear

  pure subroutine assertion_result_destroy(this)
    class(assertion_result), intent(inout) :: this
    call this%clear()
  end subroutine assertion_result_destroy

end module mod_assertion_result
