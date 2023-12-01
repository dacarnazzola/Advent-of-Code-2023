module day_01
implicit none
private

    character(len=*), parameter :: zero = 'zero', one = 'one', two = 'two', three = 'three', four = 'four', five = 'five', &
                                   six = 'six', seven = 'seven', eight = 'eight', nine = 'nine'

    public :: solve_day_01

    contains

        impure subroutine solve_day_01(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, line_len, i, ans_line, ans, ans2
            character(len=1024) :: buffer
            character(len=2) :: ans_line_str
            logical :: is_valid
            character(len=1) :: value_as_character
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            read(fid, '(a)', iostat=io) buffer
            ans = 0
            ans2 = 0
            do while (io == 0)
                line_len = len_trim(buffer)
                !! part 1
                search_forward_1: do i=1,line_len
                    if ((iachar(buffer(i:i)) >= 48) .and. (iachar(buffer(i:i)) <= 57)) then
                        ans_line_str(1:1) = buffer(i:i)
                        exit search_forward_1
                    end if
                end do search_forward_1
                search_backward_1: do i=line_len,1,-1
                    if ((iachar(buffer(i:i)) >= 48) .and. (iachar(buffer(i:i)) <= 57)) then
                        ans_line_str(2:2) = buffer(i:i)
                        exit search_backward_1
                    end if
                end do search_backward_1
                read(ans_line_str,'(i2)') ans_line
                ans = ans + ans_line
                !! part 2
                search_forward_2: do i=1,line_len
                    call valid_character(buffer(i:line_len), is_valid, value_as_character)
                    if (is_valid) then
                        ans_line_str(1:1) = value_as_character
                        exit search_forward_2
                    end if
                end do search_forward_2
                search_backward_2: do i=line_len,1,-1
                    call valid_character(buffer(i:line_len), is_valid, value_as_character)
                    if (is_valid) then
                        ans_line_str(2:2) = value_as_character
                        exit search_backward_2
                    end if
                end do search_backward_2
                read(ans_line_str,'(i2)') ans_line
                ans2 = ans2 + ans_line
                !! read next line
                read(fid, '(a)', iostat=io) buffer
            end do
            close(fid)
            write(*,'(a,i0,a,i0)') 'Day 01, part 1: ',ans,', part 2: ',ans2
        end subroutine solve_day_01

        pure subroutine valid_character(buffer, is_valid, value_as_character)
            character(len=*), intent(in) :: buffer
            logical, intent(out) :: is_valid
            character(len=1), intent(out) :: value_as_character
            integer :: buffer_len
            value_as_character = ''
            if ((iachar(buffer(1:1)) >= 48) .and. (iachar(buffer(1:1)) <= 57)) then
                value_as_character = buffer(1:1)
            else
                buffer_len = len(buffer)
                if (buffer_len >= 5) then
                    if (buffer(1:4) == zero) then
                        value_as_character = '0'
                    else if (buffer(1:3) == one) then
                        value_as_character = '1'
                    else if (buffer(1:3) == two) then
                        value_as_character = '2'
                    else if (buffer(1:5) == three) then
                        value_as_character = '3'
                    else if (buffer(1:4) == four) then
                        value_as_character = '4'
                    else if (buffer(1:4) == five) then
                        value_as_character = '5'
                    else if (buffer(1:3) == six) then
                        value_as_character = '6'
                    else if (buffer(1:5) == seven) then
                        value_as_character = '7'
                    else if (buffer(1:5) == eight) then
                        value_as_character = '8'
                    else if (buffer(1:4) == nine) then
                        value_as_character = '9'
                    end if
                else if (buffer_len >= 4) then
                    if (buffer(1:4) == zero) then
                        value_as_character = '0'
                    else if (buffer(1:3) == one) then
                        value_as_character = '1'
                    else if (buffer(1:3) == two) then
                        value_as_character = '2'
                    else if (buffer(1:4) == four) then
                        value_as_character = '4'
                    else if (buffer(1:4) == five) then
                        value_as_character = '5'
                    else if (buffer(1:3) == six) then
                        value_as_character = '6'
                    else if (buffer(1:4) == nine) then
                        value_as_character = '9'
                    end if
                else if (buffer_len >= 3) then
                    if (buffer(1:3) == one) then
                        value_as_character = '1'
                    else if (buffer(1:3) == two) then
                        value_as_character = '2'
                    else if (buffer(1:3) == six) then
                        value_as_character = '6'
                    end if
                end if
            end if
            is_valid = value_as_character /= ''
        end subroutine valid_character

end module day_01
