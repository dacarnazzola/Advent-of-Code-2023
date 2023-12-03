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

        impure function valid_gear_ratio(check_rows, j_gear) result(num_out)
            character(len=input_line_len+2), intent(in) :: check_rows(3)
            integer, intent(in) :: j_gear
            integer :: num_out, i, start_ii, stop_ii, numbers(2)
            num_out = 0
            numbers = -1
            stop_ii = -1
            if (is_number(check_rows(1)((j_gear-1):(j_gear-1)))) then !! top left corner is a number
                start_ii = j_gear - 1
                top_left_1: do i=j_gear-2,1,-1 !! back up to find the beginning
                    if (.not.is_number(check_rows(1)(i:i))) then
                        start_ii = i
                        exit top_left_1
                    end if
                end do top_left_1
                stop_ii = j_gear - 1
                top_left_2: do i=j_gear-1,len(check_rows(1)) !! roll forward to find the end
                    if (.not.is_number(check_rows(1)(i:i))) then
                        stop_ii = i
                        exit top_left_2
                end do top_left_2
                read(check_rows(1)(start_ii:stop_ii),*) numbers(1)
            end if
            if ((stop_ii < j_gear) .and. is_number(check_rows(1)(j_gear:j_gear)) then !! need to check top middle
                start_ii = j_gear
                top_middle_1: do i=j_gear-1,1,-1 !! back up to find the beginning
                    if (.not.is_number(check_rows(1)(i:i))) then
                        start_ii = i
                        exit top_middle_1
                    end if
                end do top_middle_1
                stop_ii = j_gear
                top_middle_2: do i=j_gear,len(check_rows(1)) !! roll forward to find the end
                    if (.not.is_number(check_rows(1)(i:i))) then
                        stop_ii = i
                        exit top_middle_2
                end do top_middle_2
                if (numbers(1) > -1) then
                    read(check_rows(1)(start_ii:stop_ii),*) numbers(2)
                else
                    read(check_rows(1)(start_ii:stop_ii),*) numbers(1)
                end if
            end if
            if ((stop_ii < (j_gear + 1)) .and. is_number(check_rows(1)((j_gear+1):(j_gear+1)))) then !! need to check top right corner
                if (numbers(2) > -1) exit
                start_ii = j_gear + 1
                top_right_1: do i=j_gear,1,-1 !! back up to find the beginning
                    if (.not.is_number(check_rows(1)(i:i))) then
                        start_ii = i
                        exit top_right_1
                    end if
                end do top_right_1
                stop_ii = j_gear + 1
                top_right_2: do i=j_gear+1,len(check_rows(1)) !! roll forward to find the end
                    if (.not.is_number(check_rows(1)(i:i))) then
                        stop_ii = i
                        exit top_right_2
                end do top_right_2
                if (numbers(1) > -1) then
                    read(check_rows(1)(start_ii:stop_ii),*) numbers(2)
                else
                    read(check_rows(1)(start_ii:stop_ii),*) numbers(1)
                end if
            end if
            if ((numbers(1) > -1) .and. (numbers(2) > -1)) num_out = numbers(1)*numbers(2)
        end function valid_gear_ratio

        pure logical function is_number(character_in)
            character(len=1), intent(in) :: character_in
            is_number = (iachar(character_in) >= 48) .and. (iachar(character_in) <= 57)
        end function is_number

end module day_03
