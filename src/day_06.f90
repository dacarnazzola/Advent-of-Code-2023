module day_06
use, intrinsic :: iso_fortran_env, only: i64 => int64, dp => real64
use, non_intrinsic :: string_utils, only: is_number
implicit none
private

    public :: solve_day_06

    contains

        impure subroutine solve_day_06(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, i, number_string_len
            integer(i64) :: ans1, ans2, times(5), distances(5), range_count(5)
            character(len=1024) :: buffer, number_string
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            ans1 = 0
            ans2 = 0
            do while (io == 0)
                read(unit=fid, fmt='(a)', iostat=io) buffer
                if (io == 0) then
                    if (buffer(1:4) == 'Time') then
                        read(buffer(10:),'(i6,3i7)') times(1:4)
                        number_string_len = 0
                        do i=10,len_trim(buffer)
                            if (is_number(buffer(i:i))) then 
                                number_string_len = number_string_len + 1
                                number_string(number_string_len:number_string_len) = buffer(i:i)
                            end if
                        end do
                        read(number_string(1:number_string_len),*) times(5)
                    else if (buffer(1:8) == 'Distance') then
                        read(buffer(10:),'(i6,3i7)') distances(1:4)
                        number_string_len = 0
                        do i=10,len_trim(buffer)
                            if (is_number(buffer(i:i))) then 
                                number_string_len = number_string_len + 1
                                number_string(number_string_len:number_string_len) = buffer(i:i)
                            end if
                        end do
                        read(number_string(1:number_string_len),*) distances(5)
                    else
                        write(*,*) buffer
                        error stop 'unhandled line'
                    end if
                end if
            end do
            close(fid)
            !! solve: distance < (time - speed)*speed --> -speed**2 + time*speed - distance > 0
            do i=1,size(times)
                call find_range(times(i), distances(i), range_count(i))
            end do
            ans1 = product(range_count(1:4))
            ans2 = range_count(5)
            write(*,'(a,i0,a,i0)') 'Day 06, part 1: ',ans1,', part 2: ',ans2
        end subroutine solve_day_06

        pure subroutine find_range(time, distance, range_count)
            integer(i64), intent(in) :: time, distance
            integer(i64), intent(out) :: range_count
            real(dp) :: b, c
            b = real(time, dp)
            c = real(distance, dp)
            range_count = floor((-b + sqrt(b**2 - 4.0_dp*c))/2.0_dp) - floor((-b - sqrt(b**2 - 4.0_dp*c))/2.0_dp)
        end subroutine find_range

end module day_06
