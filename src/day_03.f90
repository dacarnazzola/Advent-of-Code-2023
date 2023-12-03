module day_03
implicit none
private

    integer, parameter :: input_line_len = 140, max_string_len = 32

    public :: solve_day_03

    contains

        impure subroutine solve_day_03(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, i, ans1, ans2, j, string_len, part_number, number_jj(max_string_len)
            character(len=input_line_len+2) :: buffer, file_contents(142), check_rows(3)
            character(len=max_string_len) :: string, check_section
            logical :: in_number
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            i = 1
            file_contents(i) = repeat('.', len(file_contents(i)))
            do while (io == 0)
                read(unit=fid, fmt='(a)', iostat=io) buffer
                if (io == 0) then
                    i = i + 1
                    file_contents(i) = '.'//trim(adjustl(buffer))//'.'
                end if
            end do
            close(fid)
            file_contents(i+1) = repeat('.', len(file_contents(i+1)))
            ans1 = 0
            ans2 = 0
            do i=2,size(file_contents)-1 !! scan each row
                in_number = .false.
                string_len = 0
                do j=2,len(file_contents(i)) !! scan within each row looking for numbers
                    if (is_number(file_contents(i)(j:j))) then
                        in_number = .true.
                        string_len = string_len + 1
                        string(string_len:string_len) = file_contents(i)(j:j)
                        number_jj(string_len) = j
                    else if (in_number) then !! passed the end of a number
                        read(string(1:string_len),*) part_number
                        check_section = file_contents(i-1)((number_jj(1)-1):(number_jj(string_len)+1))// &        !! line above
                                        file_contents(i)((number_jj(1)-1):(number_jj(1)-1))// &                   !! character before
                                        file_contents(i)((number_jj(string_len)+1):(number_jj(string_len)+1))// & !! character after
                                        file_contents(i+1)((number_jj(1)-1):(number_jj(string_len)+1))            !! line below
                        ans1 = ans1 + valid_part_number(trim(check_section), part_number)
                        in_number = .false.
                        string_len = 0
                    end if
                    if (file_contents(i)(j:j) == '*') then !! found a potential gear
                        check_rows = file_contents((i-1):(i+1)) 
                        ans2 = ans2 + valid_gear_ratio(check_rows, j)
                    end if
                end do
            end do
            write(*,'(a,i0,a,i0)') 'Day 03, part 1: ',ans1,', part 2: ',ans2
        end subroutine solve_day_03

        pure function valid_part_number(check_section, part_number) result(num_out)
            character(len=*), intent(in) ::  check_section
            integer, intent(in) :: part_number
            integer :: num_out, i
            num_out = 0
            do i=1,len(check_section)
                if ((check_section(i:i) /= '.') .and. (.not.is_number(check_section(i:i)))) then
                    num_out = part_number
                    exit
                end if
            end do
        end function valid_part_number

        pure function valid_gear_ratio(check_rows, j_gear) result(num_out)
            character(len=input_line_len+2), intent(in) :: check_rows(3)
            integer, intent(in) :: j_gear
            integer :: num_out, stop_ii, numbers(2)
            num_out = 0
            numbers = -1
            stop_ii = -1
            !! check top left corner
            if (is_number(check_rows(1)((j_gear-1):(j_gear-1)))) call number_in_line_info(check_rows(1), &
                                                                                          j_gear-1, &
                                                                                          stop_ii, &
                                                                                          numbers(1))
            !! if no number in top left corner, there may be in top middle
            if ((numbers(1) < 0) .and. is_number(check_rows(1)(j_gear:j_gear))) call number_in_line_info(check_rows(1), &
                                                                                                         j_gear, &
                                                                                                         stop_ii, &
                                                                                                         numbers(1))
            !! numbers could be in top left or middle, check top right and make sure stop_ii < j_gear (1 left from top right)
            if ((stop_ii < j_gear) .and. is_number(check_rows(1)((j_gear+1):(j_gear+1)))) then
                if (numbers(1) < 0) then
                    call number_in_line_info(check_rows(1)((j_gear+1):), 1, stop_ii, numbers(1))
                else if (numbers(2) < 0) then
                    call number_in_line_info(check_rows(1)((j_gear+1):), 1, stop_ii, numbers(2))
                else
                    error stop 'shouldn''t ever get here... top right'
                end if
            end if
            !! check left of gear
            if (is_number(check_rows(2)((j_gear-1):(j_gear-1)))) then
                if (numbers(2) > -1) then
                    return !! early return, but there must be EXACTLY two adjacent numbers for a gear ratio
                else if (numbers(1) < 0) then
                    call number_in_line_info(check_rows(2)(1:(j_gear-1)), j_gear - 1, stop_ii, numbers(1))
                else if (numbers(2) < 0) then
                    call number_in_line_info(check_rows(2)(1:(j_gear-1)), j_gear - 1, stop_ii, numbers(2))
                else
                    error stop 'shouldn''t ever get here... middle left'
                end if
            end if
            !! check right of gear
            if (is_number(check_rows(2)((j_gear+1):))) then
                if (numbers(2) > -1) then
                    return !! early return, but there must be EXACTLY two adjacent numbers for a gear ratio
                else if (numbers(1) < 0) then
                    call number_in_line_info(check_rows(2)((j_gear+1):), 1, stop_ii, numbers(1))
                else if (numbers(2) < 0) then
                    call number_in_line_info(check_rows(2)((j_gear+1):), 1, stop_ii, numbers(2))
                else
                    error stop 'shouldn''t ever get here... middle right'
                end if
            end if
            !! check bottom left
            if (is_number(check_rows(3)((j_gear-1):(j_gear-1)))) then
                if (numbers(2) > -1) then
                    return !! early return, but there must be EXACTLY two adjacent numbers for a gear ratio
                else if (numbers(1) < 0) then
                    call number_in_line_info(check_rows(3), j_gear - 1, stop_ii, numbers(1))
                else if (numbers(2) < 0) then
                    call number_in_line_info(check_rows(3), j_gear - 1, stop_ii, numbers(2))
                else
                    error stop 'shouldn''t ever get here... bottom left'
                end if
            end if
            !! check bottom middle
            if (is_number(check_rows(3)(j_gear:j_gear)) .and. (.not.(is_number(check_rows(3)((j_gear-1):(j_gear-1)))))) then
                if (numbers(2) > -1) then
                    return !! early return, but there must be EXACTLY two adjacent numbers for a gear ratio
                else if (numbers(1) < 0) then
                    call number_in_line_info(check_rows(3), j_gear, stop_ii, numbers(1))
                else if (numbers(2) < 0) then
                    call number_in_line_info(check_rows(3), j_gear, stop_ii, numbers(2))
                else
                    error stop 'shouldn''t ever get here... bottom middle'
                end if
            end if
            !! check bottom right
            if (is_number(check_rows(3)((j_gear+1):(j_gear+1))) .and. (.not.(is_number(check_rows(3)(j_gear:j_gear))))) then
                if ((numbers(2) > -1) .or. (numbers(1) < 0)) then
                    return !! early return, but there must be EXACTLY two adjacent numbers for a gear ratio
                else if (numbers(2) < 0) then
                    call number_in_line_info(check_rows(3)((j_gear+1):), 1, stop_ii, numbers(2))
                else
                    error stop 'shouldn''t ever get here... bottom right'
                end if
            end if
            if ((numbers(1) > -1) .and. (numbers(2) > -1)) num_out = numbers(1)*numbers(2)
        end function valid_gear_ratio

        pure logical function is_number(character_in)
            character(len=1), intent(in) :: character_in
            is_number = (iachar(character_in) >= 48) .and. (iachar(character_in) <= 57)
        end function is_number

        pure subroutine number_in_line_info(line, number_ii, stop_ii, num_out)
            character(len=*), intent(in) :: line
            integer, intent(in) :: number_ii
            integer, intent(out) :: stop_ii, num_out
            integer :: i, start_ii
            do i=number_ii,1,-1 !! check backwards
                if (is_number(line(i:i))) then
                    start_ii = i
                else
                    exit
                end if
            end do
            do i=number_ii,len(line) !! check forwards
                if (is_number(line(i:i))) then
                    stop_ii = i
                else
                    exit
                end if
            end do
            read(line(start_ii:stop_ii),*) num_out
        end subroutine number_in_line_info

end module day_03
