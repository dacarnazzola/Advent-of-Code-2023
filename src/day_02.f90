module day_02
implicit none
private

    integer, parameter :: max_r = 12, max_g = 13, max_b = 14
    character(len=*), parameter :: red = 'red', green = 'green', blue = 'blue'

    public :: solve_day_02

    contains

        impure subroutine solve_day_02(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, game_id, game_power, ans1, ans2
            character(len=1024) :: buffer
            logical :: game_possible
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            read(fid, '(a)', iostat=io) buffer
            ans1 = 0
            ans2 = 0
            do while (io == 0)
                !! parse line
                call parse_line(buffer(1:len_trim(buffer)), game_possible, game_id, game_power)
                if (game_possible) ans1 = ans1 + game_id
                ans2 = ans2 + game_power
                !! read next line
                read(fid, '(a)', iostat=io) buffer
            end do
            close(fid)
            write(*,'(a,i0,a,i0)') 'Day 02, part 1: ',ans1,', part 2: ',ans2
        end subroutine solve_day_02

        pure subroutine parse_line(line, game_possible, game_id, game_power)
            character(len=*), intent(in) :: line
            logical, intent(out) :: game_possible
            integer, intent(out) :: game_id, game_power
            integer :: line_len, i, semicolon_ii, num_balls, string_len, min_rgb(3)
            character(len=5) :: string
            logical :: look_for_number, look_for_color
            line_len = len(line)
            find_semicolon_ii: do i=1,line_len
                if (line(i:i) == ':') then
                    semicolon_ii = i
                    read(line(6:(semicolon_ii-1)),*) game_id
                    exit find_semicolon_ii
                end if
            end do find_semicolon_ii
            look_for_number = .true.
            look_for_color = .false.
            string_len = 0
            game_possible = .true.
            min_rgb = 0
            check_game: do i=semicolon_ii+2,line_len
                if (look_for_number) then
                    if ((iachar(line(i:i)) >= 48) .and. (iachar(line(i:i)) <= 57)) then
                        string_len = string_len + 1
                        string(string_len:string_len) = line(i:i)
                    else if ((string_len > 0) .and. (line(i:i) == ' ')) then
                        read(string(1:string_len),*) num_balls
                        look_for_number = .false.
                        look_for_color = .true.
                        string_len = 0
                    end if
                else if (look_for_color) then
                    if ((line(i:i) /= ' ') .and. (line(i:i) /= ';') .and. (line(i:i) /= ',')) then
                        string_len = string_len + 1
                        string(string_len:string_len) = line(i:i)
                    else if ((string_len > 0) .and. ((line(i:i) == ' ') .or. (line(i:i) == ';') .or. (line(i:i) == ','))) then
                        if (string(1:string_len) == red) then
                            min_rgb(1) = max(num_balls, min_rgb(1))
                            if (num_balls > max_r) game_possible = .false.
                        else if (string(1:string_len) == green) then
                            min_rgb(2) = max(num_balls, min_rgb(2))
                            if (num_balls > max_g) game_possible = .false.
                        else if (string(1:string_len) == blue) then
                            min_rgb(3) = max(num_balls, min_rgb(3))
                            if (num_balls > max_b) game_possible = .false.
                        end if
                        look_for_number = .true.
                        look_for_color = .false.
                        string_len = 0
                    end if
                end if
            end do check_game
            if (string(1:string_len) == red) then
                min_rgb(1) = max(num_balls, min_rgb(1))
                if (num_balls > max_r) game_possible = .false.
            else if (string(1:string_len) == green) then
                min_rgb(2) = max(num_balls, min_rgb(2))
                if (num_balls > max_g) game_possible = .false.
            else if (string(1:string_len) == blue) then
                min_rgb(3) = max(num_balls, min_rgb(3))
                if (num_balls > max_b) game_possible = .false.
            end if
            game_power = min_rgb(1)*min_rgb(2)*min_rgb(3)
        end subroutine parse_line

end module day_02
