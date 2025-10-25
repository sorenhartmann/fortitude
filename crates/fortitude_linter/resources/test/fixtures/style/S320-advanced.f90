module test_unused_symbols

contains

  subroutine compute_sum(a, b, n)
    integer, intent(in) :: a, b, n
    integer :: tmp

    !$omp private(tmp, b) shared(a)
    print*, a + b
    !$omp end parallel
  end subroutine compute_sum

  subroutine compute_sum(a, b, n)
    integer, intent(in) :: a, b, n
    integer :: tmp

    !$omp parallel num_threads (n) private(tmp, b) shared(a)
    print*, a + b
    !$omp end parallel
  end subroutine compute_sum

end module test_unused_symbols
