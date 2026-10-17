module mod_assertion_result
  implicit none
  private

  integer, parameter, public :: ASSERTION_DETAIL_LEN = 512
  integer, parameter, public :: ASSERTION_METHOD_LEN = 64

  type, public :: assertion_failure
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

  subroutine assertion_result_push(this, flat_index, detail)
    class(assertion_result), intent(inout) :: this
    integer, intent(in) :: flat_index
    character(*), intent(in) :: detail
    type(assertion_failure), allocatable :: grown(:)
    integer :: capacity, new_capacity

    if (.not. allocated(this%details)) then
      allocate(this%details(1))
    else if (this%n_details == size(this%details)) then
      capacity = size(this%details)
      new_capacity = max(1, 2 * capacity)
      allocate(grown(new_capacity))
      grown(1:this%n_details) = this%details(1:this%n_details)
      call move_alloc(grown, this%details)
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

  subroutine assertion_result_clear(this)
    class(assertion_result), intent(inout) :: this
    if (allocated(this%details)) deallocate(this%details)
    if (allocated(this%shape_a)) deallocate(this%shape_a)
    if (allocated(this%shape_b)) deallocate(this%shape_b)
    this%n_details = 0
    this%method = ''
  end subroutine assertion_result_clear

  subroutine assertion_result_finalize(this)
    type(assertion_result), intent(inout) :: this
    call this%clear()
  end subroutine assertion_result_finalize

end module mod_assertion_result
