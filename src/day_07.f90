module day_07
use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none
private

    public :: solve_day_07

    contains

        impure subroutine solve_day_07(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io, ans1, ans2, bets(1000), i, ii(1000), num_hands
            character(len=1024) :: buffer
            character(len=5) :: hands(1000)
            real(dp) :: scores(1000)
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            ans1 = 0
            ans2 = 0
            num_hands = 0
            do while (io == 0)
                read(unit=fid, fmt='(a)', iostat=io) buffer
                if (io == 0) then
                    num_hands = num_hands + 1
                    read(buffer(1:5),'(a)') hands(num_hands)
                    read(buffer(6:),*) bets(num_hands)
                    ii(num_hands) = num_hands
                end if
            end do
            close(fid)
            call score_hands(hands(1:num_hands), scores(1:num_hands))
            call sort(scores(1:num_hands), ii(1:num_hands))
            do i=1,num_hands
                ans1 = ans1 + i*bets(ii(i))
                ii(i) = i
            end do
            call score_hands2(hands(1:num_hands), scores(1:num_hands))
            call sort(scores(1:num_hands), ii(1:num_hands))
            do i=1,num_hands
                ans2 = ans2 + i*bets(ii(i))
            end do
            write(*,'(a,i0,a,i0)') 'Day 07, part 1: ',ans1,', part 2: ',ans2
        end subroutine solve_day_07

        impure elemental subroutine score_hands2(hand, score)
            character(len=5), intent(in) :: hand
            real(dp), intent(out) :: score
            integer :: i, c(13), jokers
            score = 0.0_dp
            c = 0
            jokers = 0
            do i=1,5
                if (hand(i:i) == 'A') then
                    c(1) = c(1) + 1
                    score = score + 0.9_dp/10**(i*2-1)
                else if (hand(i:i) == 'K') then
                    c(2) = c(2) + 1
                    score = score + 0.8333_dp/10**(i*2-1)
                else if (hand(i:i) == 'Q') then
                    c(3) = c(3) + 1
                    score = score + 0.7666_dp/10**(i*2-1)
                else if (hand(i:i) == 'J') then
                    jokers = jokers + 1
                    score = score + 0.1_dp/10**(i*2-1)
                else if (hand(i:i) == 'T') then
                    c(5) = c(5) + 1
                    score = score + 0.7_dp/10**(i*2-1)
                else if (hand(i:i) == '9') then
                    c(6) = c(6) + 1
                    score = score + 0.6333_dp/10**(i*2-1)
                else if (hand(i:i) == '8') then
                    c(7) = c(7) + 1
                    score = score + 0.5666_dp/10**(i*2-1)
                else if (hand(i:i) == '7') then
                    c(8) = c(8) + 1
                    score = score + 0.5_dp/10**(i*2-1)
                else if (hand(i:i) == '6') then
                    c(9) = c(9) + 1
                    score = score + 0.4333_dp/10**(i*2-1)
                else if (hand(i:i) == '5') then
                    c(10) = c(10) + 1
                    score = score + 0.3666_dp/10**(i*2-1)
                else if (hand(i:i) == '4') then
                    c(11) = c(11) + 1
                    score = score + 0.3_dp/10**(i*2-1)
                else if (hand(i:i) == '3') then
                    c(12) = c(12) + 1
                    score = score + 0.2333_dp/10**(i*2-1)
                else if (hand(i:i) == '2') then
                    c(13) = c(13) + 1
                    score = score + 0.1666_dp/10**(i*2-1)
                end if
            end do
            select case (jokers)
                case (0)
                    if (any(c == 5)) then !! 5 of a kind
                        score = score + 7.0_dp
                    else if (any(c == 4)) then !! 4 of a kind
                        score = score + 6.0_dp
                    else if (any(c == 3) .and. any(c == 2)) then !! full house
                        score = score + 5.0_dp
                    else if (any(c == 3)) then !! 3 of a kind
                        score = score + 4.0_dp
                    else if (count(c == 2) == 2) then !! 2 pair
                        score = score + 3.0_dp
                    else if (any(c == 2)) then !! 1 pair
                        score = score + 2.0_dp
                    else !! high card
                        score = score + 1.0_dp
                    end if
                case (1)
                    if (any(c == 4)) then !! 4 of a kind promoted
                        score = score + 7.0_dp
                    else if (any(c == 3)) then !! 3 of a kind promoted
                        score = score + 6.0_dp
                    else if (count(c == 2) == 2) then !! two pair promoted
                        score = score + 5.0_dp
                    else if (any(c == 2)) then !! 1 pair promoted
                        score = score + 4.0_dp
                    else if (count(c == 2) == 1) then !! 1 pair
                        score = score + 3.0_dp
                    else if (maxval(c) == 1) then !! high card promoted
                        score = score + 2.0_dp
                    else !! high card
                        error stop 'should all be caught in 1 joker...'
                    end if
                case (2)
                    if (any(c == 3)) then !! 3 of a kind promoted
                        score = score + 7.0_dp
                    else if (any(c == 2)) then !! 2 of a kind promoted
                        score = score + 6.0_dp
                    else if (count(c == 2) == 2) then !! 2 jokers and 1 other pair, 1 loose --> 3 of a kind + 2 of a kind
                        score = score + 5.0_dp
                    else if (maxval(c) == 1) then !! 2 jokers + 3 loose --> three of a kind
                        score = score + 4.0_dp
                    else
                        error stop 'should all be caught in 2 jokers'
                    end if 
                case (3)
                    if (any(c == 2)) then !! pair promoted to 5 of a kind
                        score = score + 7.0_dp
                    else if (maxval(c) == 1) then !! 3 jokers + 2 loose --> 4 of a kind
                        score = score + 6.0_dp
                    else
                        error stop 'should all be caught 3 jokers...'
                    end if
                case (4)
                    score = score + 7.0_dp
                case (5)
                    score = score + 7.0_dp
                case default
                    error stop 'jokers < 0 or jokers > 5'
            end select
        end subroutine score_hands2

        pure elemental subroutine score_hands(hand, score)
            character(len=5), intent(in) :: hand
            real(dp), intent(out) :: score
            integer :: i, c(13)
            score = 0.0_dp
            c = 0
            do i=1,5
                if (hand(i:i) == 'A') then
                    c(1) = c(1) + 1
                    score = score + 0.9_dp/10**(i*2-1)
                else if (hand(i:i) == 'K') then
                    c(2) = c(2) + 1
                    score = score + 0.8333_dp/10**(i*2-1)
                else if (hand(i:i) == 'Q') then
                    c(3) = c(3) + 1
                    score = score + 0.7666_dp/10**(i*2-1)
                else if (hand(i:i) == 'J') then
                    c(4) = c(4) + 1
                    score = score + 0.7_dp/10**(i*2-1)
                else if (hand(i:i) == 'T') then
                    c(5) = c(5) + 1
                    score = score + 0.6333_dp/10**(i*2-1)
                else if (hand(i:i) == '9') then
                    c(6) = c(6) + 1
                    score = score + 0.5666_dp/10**(i*2-1)
                else if (hand(i:i) == '8') then
                    c(7) = c(7) + 1
                    score = score + 0.5_dp/10**(i*2-1)
                else if (hand(i:i) == '7') then
                    c(8) = c(8) + 1
                    score = score + 0.4333_dp/10**(i*2-1)
                else if (hand(i:i) == '6') then
                    c(9) = c(9) + 1
                    score = score + 0.3666_dp/10**(i*2-1)
                else if (hand(i:i) == '5') then
                    c(10) = c(10) + 1
                    score = score + 0.3_dp/10**(i*2-1)
                else if (hand(i:i) == '4') then
                    c(11) = c(11) + 1
                    score = score + 0.2333_dp/10**(i*2-1)
                else if (hand(i:i) == '3') then
                    c(12) = c(12) + 1
                    score = score + 0.1666_dp/10**(i*2-1)
                else if (hand(i:i) == '2') then
                    c(13) = c(13) + 1
                    score = score + 0.1_dp/10**(i*2-1)
                end if
            end do
            if (any(c == 5)) then !! 5 of a kind
                score = score + 7.0_dp
            else if (any(c == 4)) then !! 4 of a kind
                score = score + 6.0_dp
            else if (any(c == 3) .and. any(c == 2)) then !! full house
                score = score + 5.0_dp
            else if (any(c == 3)) then !! 3 of a kind
                score = score + 4.0_dp
            else if (count(c == 2) == 2) then !! 2 pair
                score = score + 3.0_dp
            else if (any(c == 2)) then !! 1 pair
                score = score + 2.0_dp
            else !! high card
                score = score + 1.0_dp
            end if
        end subroutine score_hands

        pure subroutine sort(vec, vec2) !! bubble sort because I don't have any better ones memorized.
        !! TODO: memorize dual pivot quick sort
            real(dp), intent(inout) :: vec(:)
            integer, intent(inout) :: vec2(:)
            real(dp) :: swap
            integer :: i, j, swap2
            do i=1,size(vec)
                do j=2,size(vec)
                    if (vec(j-1) > vec(j)) then
                        swap = vec(j)
                        vec(j) = vec(j-1)
                        vec(j-1) = swap
                        swap2 = vec2(j)
                        vec2(j) = vec2(j-1)
                        vec2(j-1) = swap2
                    end if
                end do
            end do
        end subroutine sort 

end module day_07
