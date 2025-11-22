!=========================================================
! Main test module
!=========================================================
module test_unused_symbols

  use itertools, only: reduce
  use math_utils, only: square, cube, normalize, print_banner
  use kind_module, only: long, short

  public :: g_i

  implicit none

  abstract interface
    function g_i(x) result(y)
      real, intent(in) :: x
      real :: y
      ! unused is allowed in interfaces
    end function g_i
  end interface

contains

  subroutine compute_sum(a, b)
    integer, intent(in) :: a, b
    integer :: tmp
    print *, "sum:", a + b
    global_usEd = globAl_used + 1
  end subroutine compute_sum

  subroutine compute_diff(aA, b)
    integer, intent(in) :: aa, b
    integer :: diff
    diff = AA - b
    print *, "diff:", diff
  end subroutine compute_diff

  function add_one(x, y) result(r)
    integer, intent(in) :: x, Y
    integer :: r
    r = x + 1
  end function add_one

  function used_interface(f) result(res)
    implicit none
    integer :: res
    interface
      function f(x) result(y)
        real, intent(in) :: x
        real :: y
      end function f
    end interface
    print*, f(1.) ! used
  end function used_interface

  function unused_interface(f) result(res)
    implicit none
    integer :: res
    interface
      function f(x) result(y)
        real, intent(in) :: x
        real :: y
      end function f
    end interface
    ! unused
  end function unused_interface


  !---------------------------------------------------------
  ! 7. Larger method: simulate process
  !   Demonstrates a local USE statement
  ! Expect: unused_temp, unused_counter unused
  !---------------------------------------------------------
  subroutine simulate_process(steps, rate)
    use math_utils, only: cube, normalize   ! Local import
    implicit none
    integer, intent(in) :: steps
    real(kind=long), intent(in) :: rate
    integer :: i
    integer :: unused_counter, def
    real :: ghi, state, unused_temp, abc
    state = 0.0
    do i = 1, steps
      state = state + normalize(cube(rate * i), 100.0)
    end do
    print *, "final state:", state
  end subroutine simulate_process

  subroutine demo_math()
    real :: val, res1, res2
    val = 3.0
    res1 = square(val)          ! used import
    res2 = normalize(val, 10.0) ! used import
    print *, "math demo:", res1, res2
  end subroutine demo_math

  subroutine f1
   integer :: a, b, c
   print*, a
   end subroutine

  subroutine f2
   integer :: a, b, c
   print*, b
   end subroutine

  subroutine f3
   integer :: a, b, c
   print*, c
   end subroutine

  subroutine g0
   integer :: &
      a, &
      b, &
      c
   print*, "nothing used"
   end subroutine

  subroutine g1
   integer :: &
      a, &
      b, &
      c
   print*, a
   end subroutine

  subroutine g2
   integer :: &
      a, &
      b, &
      c
   print*, b
   end subroutine

  subroutine g3
   integer :: &
      a, &
      b, &
      c
   print*, c
   end subroutine

end module test_unused_symbols
