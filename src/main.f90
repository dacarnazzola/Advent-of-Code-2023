program aoc2023
use, non_intrinsic :: day_01, only: solve_day_01
use, non_intrinsic :: day_02, only: solve_day_02
use, non_intrinsic :: day_03, only: solve_day_03
use, non_intrinsic :: day_04, only: solve_day_04
use, non_intrinsic :: day_05, only: solve_day_05
use, intrinsic :: iso_fortran_env, only: int64, real64
implicit none

    integer(int64) :: c1, cr, c2

    call system_clock(c1, cr)
    call solve_day_01('./inputs/day_01_input.txt')
    call system_clock(c2)
    write(*,'(a,f0.1,a)') 'solved DAY_01 in ',real(max(c2 - c1, 1_int64), real64)/real(cr, real64)*1000.0_real64,' ms'

    call system_clock(c1, cr)
    call solve_day_02('./inputs/day_02_input.txt')
    call system_clock(c2)
    write(*,'(a,f0.1,a)') 'solved DAY_02 in ',real(max(c2 - c1, 1_int64), real64)/real(cr, real64)*1000.0_real64,' ms'

    call system_clock(c1, cr)
    call solve_day_03('./inputs/day_03_input.txt')
    call system_clock(c2)
    write(*,'(a,f0.1,a)') 'solved DAY_03 in ',real(max(c2 - c1, 1_int64), real64)/real(cr, real64)*1000.0_real64,' ms'

    call system_clock(c1, cr)
    call solve_day_04('./inputs/day_04_input.txt')
    call system_clock(c2)
    write(*,'(a,f0.1,a)') 'solved DAY_04 in ',real(max(c2 - c1, 1_int64), real64)/real(cr, real64)*1000.0_real64,' ms'

    call system_clock(c1, cr)
    call solve_day_05('./inputs/day_05_input.txt')
    call system_clock(c2)
    write(*,'(a,f0.1,a)') 'solved DAY_05 in ',real(max(c2 - c1, 1_int64), real64)/real(cr, real64)*1000.0_real64,' ms'

end program aoc2023
