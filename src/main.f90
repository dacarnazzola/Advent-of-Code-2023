program aoc2023
use, non_intrinsic :: day_01, only: solve_day_01
use, intrinsic :: iso_fortran_env, only: int64, real64
implicit none

    integer(int64) :: c1, cr, c2

    call system_clock(c1, cr)
    call solve_day_01('./inputs/day_01_input.txt')
    call system_clock(c2)
    write(*,'(a,f0.1,a)') 'solved DAY_01 in ',real(max(c2 - c1, 1_int64), real64)/real(cr, real64)*1000.0_real64,' ms'

end program aoc2023
