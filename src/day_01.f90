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
                    select case (buffer(1:3))
                        case (one)
                            value_as_character = '1'
                        case (two)
                            value_as_character = '2'
                        case (six)
                            value_as_character = '6'
                    end select
                    select case (buffer(1:4))
                        case (zero)
                            value_as_character = '0'
                        case (four)
                            value_as_character = '4'
                        case (five)
                            value_as_character = '5'
                        case (nine)
                            value_as_character = '9'
                    end select
                    select case (buffer(1:5))
                        case (three)
                            value_as_character = '3'
                        case (seven)
                            value_as_character = '7'
                        case (eight)
                            value_as_character = '8'
                    end select
                else if (buffer_len >= 4) then
                    select case (buffer(1:3))
                        case (one)
                            value_as_character = '1'
                        case (two)
                            value_as_character = '2'
                        case (six)
                            value_as_character = '6'
                    end select
                    select case (buffer(1:4))
                        case (zero)
                            value_as_character = '0'
                        case (four)
                            value_as_character = '4'
                        case (five)
                            value_as_character = '5'
                        case (nine)
                            value_as_character = '9'
                    end select
                else if (buffer_len >= 3) then
                    select case (buffer(1:3))
                        case (one)
                            value_as_character = '1'
                        case (two)
                            value_as_character = '2'
                        case (six)
                            value_as_character = '6'
                    end select
                end if
            end if
            is_valid = value_as_character /= ''
        end subroutine valid_character

end module day_01
