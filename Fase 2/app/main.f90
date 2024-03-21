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
        call admin()
      case (1)
        ! call log_in()
      case (2)
        call sign_in()
      case (3)
        running = .false.
      case default
        print *, "Invalid option"
    end select
  end do  
contains
  subroutine admin()
    implicit none
    integer :: op
    logical :: run = .true.
    logical :: access = .false.
    character(20) :: password

    do while (.not. access)
      print *, "------------------Admin log in-------------------"
      print *, "Enter your password:"
      print *, "(or type 'exit' to return to the main menu)"
      read (*, '(A)') password
      if (trim(password) == "exit") then
        return
      end if
      if (trim(password) == "admin") then
        access = .true.
      else
        print *, "Invalid password"
      end if
      print *, "-------------------------------------------------"
    end do
    
    do while (run)
      print *, "---------------------Admin-----------------------"
      print *, "Select an option:"
      print *, "0. Show B-Tree"
      print *, "1. Users operations (insert, remove, modify)"
      print *, "2. Masive charge of users"
      print *, "3. Reports"
      print *, "4. Exit"
      print *, "-------------------------------------------------"
      read *, op
      select case (op)
        case (0)
          ! call clients%generate_dot_file()
        case (1)
          ! call clients_operations()
        case (2)
          ! call open_clients_file()
        case (3)
          ! call reports()
        case (4)
          run = .false.
          print *, "Returning to the main menu..."
        case default
          print *, "Invalid option"
      end select
    end do    
  end subroutine admin
  subroutine sign_in()
    implicit none
    character(20) :: username
    character(20) :: password
    character(20) :: dpi_id 
    integer*8 :: dpi_num
    print *, "--------------------Sign in----------------------"
    print *, "Enter your username:"
    read (*, '(A)') username
    print *, "Enter your password:"
    read (*, '(A)') password
    print *, "Enter your DPI:"
    read (*, '(A)') dpi_id
    read (dpi_id, '(I13)') dpi_num
    write (*,'(A, A)') "Welcome to Pixel Print Studio, ", trim(username)
    write (*,'(A, I13)') "Your DPI is: ", dpi_num
    print *, "-------------------------------------------------"
  end subroutine sign_in
end program main
