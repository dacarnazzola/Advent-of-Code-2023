program test_string_utils
use, non_intrinsic :: string_utils, only: string, split
implicit none

    character(len=*), parameter :: test1 = '7', test2 = ' 1 2  32 312 3    21  2', test3 = '23423'
    type(string), allocatable :: split_string(:)
    integer :: i

    allocate(split_string(0))

    call split(input=test1, delimiter=' ', output=split_string)
    write(*,'(a)') 'test1: '//test1
    do i=1,size(split_string)
        write(*,'(a,i0,a)') 'split_string(',i,'): '//split_string(i)%s
    end do
    write(*,'(a)') ''

    call split(input=test2, delimiter=' ', output=split_string)
    write(*,'(a)') 'test2: '//test2
    do i=1,size(split_string)
        write(*,'(a,i0,a)') 'split_string(',i,'): '//split_string(i)%s
    end do
    write(*,'(a)') ''

    call split(input=test3, delimiter=' ', output=split_string)
    write(*,'(a)') 'test3: '//test3
    do i=1,size(split_string)
        write(*,'(a,i0,a)') 'split_string(',i,'): '//split_string(i)%s
    end do
    write(*,'(a)') ''

end program test_string_utils
