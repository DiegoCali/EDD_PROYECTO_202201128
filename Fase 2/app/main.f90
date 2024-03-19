program main
  implicit none
  integer :: option
  logical :: running = .true.

  do while (running)
    print *, "----------Welcome to Pixel Print Studio----------"
    print *, "Select an option:"
    print *, "0. Admin"
    print *, "1. Log in"
    print *, "2. Sign in"
    print *, "3. Exit"
    print *, "-------------------------------------------------"
    read *, option
    select case (option)
      case (0)
        ! call admin()
      case (1)
        ! call log_in()
      case (2)
        ! call sign_in()
      case (3)
        running = .false.
      case default
        print *, "Invalid option"
    end select
  end do  
end program main
