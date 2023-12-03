module string_utils
implicit none
private

    type string
        character(len=:), allocatable :: s
    end type string

    public :: string, split

    contains

        pure subroutine split(input, delimiter, output)
            character(len=*), intent(in) :: input
            character(len=1), intent(in) :: delimiter
            type(string), allocatable, intent(out) :: output(:)
            character(len=:), allocatable :: buffer
            integer :: len_input, i
            len_input = len(input)
            allocate(character(len=0) :: buffer)
            if (len_input > 0) then
                buffer = ''
                do i=1,len_input
                    if (input(i:i) /= delimiter) then !! character is NOT delimiter, append character to buffer
                        buffer = buffer//input(i:i)
                    else if (buffer /= '') then !! character IS delimiter and buffer is not empty, append buffer to output
                        if (.not.allocated(output)) then
                            allocate(output(1))
                            output(1) = string(buffer)
                        else
                            call append(output, string(buffer))
                        end if
                        buffer = ''
                    end if !! character is delimiter and buffer is empty, do nothing
                end do
                if (buffer /= '') then
                    if (.not.allocated(output)) then
                        allocate(output(1))
                        output(1) = string(buffer)
                    else
                        call append(output, string(buffer))
                    end if
                end if
            end if
        end subroutine split

        pure subroutine append(io, add)
            type(string), allocatable, intent(inout) :: io(:)
            type(string), intent(in) :: add
            type(string), allocatable :: work(:)
            if (allocated(io)) then
                allocate(work(size(io) + 1))
                work(1:size(io)) = io
                work(size(work)) = add
                deallocate(io)
            else
                allocate(work(1))
                work = add
            end if
            call move_alloc(work, io)
        end subroutine append

end module string_utils
