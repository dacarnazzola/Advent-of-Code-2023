module day_05
use, intrinsic :: iso_fortran_env, only: i64 => int64
use, non_intrinsic :: string_utils, only: string, split, is_number
implicit none
private

    public :: solve_day_05

    contains

        impure subroutine solve_day_05(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, i, num_seeds, map_ii, line_ii, map_length(7)
            integer(i64) :: ans1, ans2, seeds(20), maps(3,100,7)
            character(len=1024) :: buffer
            type(string), allocatable :: split_string(:)
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            ans1 = 0_i64
            ans2 = 0_i64
            num_seeds = 0
            map_ii = 0
            do while (io == 0)
                read(unit=fid, fmt='(a)', iostat=io) buffer
                if (buffer(1:5) == 'seeds') then !! read the seeds: line
                    call split(buffer, ' ', split_string)
                    do i=1,size(split_string)
                        if (is_number(split_string(i)%s(1:1))) then
                            num_seeds = num_seeds + 1
                            read(split_string(i)%s,*) seeds(num_seeds)
                        end if
                    end do
                else if ((buffer(1:5) == 'seed-') .or. (buffer(1:5) == 'soil-') .or. (buffer(1:5) == 'ferti') .or. &
                         (buffer(1:5) == 'water') .or. (buffer(1:5) == 'light') .or. (buffer(1:5) == 'tempe') .or. &
                         (buffer(1:5) == 'humid')) then !! increment map_ii
                    map_ii = map_ii + 1
                    line_ii = 0
                else !! reading a block or blank line
                    if (buffer /= '') then
                        if (allocated(split_string)) deallocate(split_string)
                        call split(buffer, ' ', split_string)
                        line_ii = line_ii + 1
                        map_length(map_ii) = line_ii
                        read(split_string(1)%s,*) maps(1, line_ii, map_ii)
                        read(split_string(2)%s,*) maps(2, line_ii, map_ii)
                        read(split_string(3)%s,*) maps(3, line_ii, map_ii)
                    end if
                end if
            end do
            close(fid)
            call score_seeds(seeds(1:num_seeds), maps, map_length, ans1)
            write(*,'(a,i0,a,i0)') 'Day 05, part 1: ',ans1,', part 2: ',ans2
        end subroutine solve_day_05

        impure subroutine score_seeds(seeds, maps, map_length, ans1)
            integer(i64), intent(in) :: seeds(:), maps(:,:,:)
            integer, intent(in) :: map_length(:)
            integer(i64), intent(out) :: ans1
            integer :: i, j, k
            integer(i64) :: input, offset, output
            ans1 = huge(1_i64)
            loop_seeds: do i=1,size(seeds)
                output = seeds(i)
                loop_maps: do j=1,size(maps, dim=3) !! go through each map
                    input = output
                    loop_lines: do k=1,map_length(j)
                        offset = input - maps(2,k,j)
                        if ((offset >= 0) .and. (input <= (maps(2,k,j) + maps(3,k,j) - 1_i64))) then
                            output = maps(1,k,j) + offset
                            exit loop_lines
                        end if
                    end do loop_lines
                end do loop_maps
                ans1 = min(ans1, output)
            end do loop_seeds
        end subroutine score_seeds

end module day_05
