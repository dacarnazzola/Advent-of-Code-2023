module day_04
implicit none
private

    integer, parameter :: file_length = 197

    public :: solve_day_04

    contains

        impure subroutine solve_day_04(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, ans1, ans2, winning_numbers(10), my_numbers(25), i, wins(file_length), score
            character(len=1024) :: buffer, lines(file_length)
            character(len=16) :: junk(2)
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            ans1 = 0
            ans2 = 0 
            i = 0
            do while (io == 0)
                read(unit=fid, fmt='(a)', iostat=io) buffer
                if (io == 0) then
                    i = i + 1
                    lines(i) = buffer
                    read(unit=lines(i), fmt='(a9,10i3,a2,25i3)') junk(1), winning_numbers, junk(2), my_numbers
                    call score_card(winning_numbers, my_numbers, wins(i), score)
                    ans1 = ans1 + score
                end if
            end do
            close(fid)
            ans2 = total_wins(wins)
            write(*,'(a,i0,a,i0)') 'Day 03, part 1: ',ans1,', part 2: ',ans2
        end subroutine solve_day_04

        pure subroutine score_card(winning_numbers, my_numbers, num_winning_numbers, score)
            integer, intent(in) :: winning_numbers(:), my_numbers(:)
            integer, intent(out) :: num_winning_numbers, score
            integer :: i, j
            num_winning_numbers = 0
            do i=1,size(my_numbers)
                do j=1,size(winning_numbers)
                    if (my_numbers(i) == winning_numbers(j)) num_winning_numbers = num_winning_numbers + 1
                end do
            end do
            score = 2**(num_winning_numbers - 1)
        end subroutine score_card

        pure integer function total_wins(wins)
            integer, intent(in) :: wins(:)
            integer :: repeats, cards_to_read(size(wins)), i
            cards_to_read = 1
            total_wins = 0
            do i=1,size(cards_to_read)
                repeats = wins(i)
                if (repeats > 0) cards_to_read(i+1:i+repeats) = cards_to_read(i+1:i+repeats) + cards_to_read(i)
                total_wins = total_wins + cards_to_read(i)
            end do
        end function total_wins

end module day_04
