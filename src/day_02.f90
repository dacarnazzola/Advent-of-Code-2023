module day_02
implicit none
private

    public :: solve_day_02

    contains

        impure subroutine solve_day_02(input_file)
            character(len=*), intent(in) :: input_file
            integer :: fid, io
            character(len=1024) :: buffer
            open(newunit=fid, file=input_file, status='old', action='read', iostat=io)
            if (io /= 0) error stop 'error opening '//input_file
            read(fid, '(a)', iostat=io) buffer
            close(fid)
        end subroutine solve_day_02

end module day_02
