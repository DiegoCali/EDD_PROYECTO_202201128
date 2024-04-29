program date_test
    implicit none
    character(20) :: timestamp
    integer, dimension(8) :: values
    call date_and_time(values=values)
    !print '(8i5)', values
    write(timestamp, '(I0, A, I0, A, I0, A, I0, A, I0, A, I0)') values(3), '-', values(2) &
        , '-', values(1), '::', values(5), ':', values(6), ':', values(7)
    print *, trim(timestamp)
end program date_test