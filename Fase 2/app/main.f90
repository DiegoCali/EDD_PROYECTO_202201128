program main
  use filehandler
  use clients
  implicit none
  type(Btree_clients), target :: global_clients
  type(client), pointer :: actual_client
  type(fhandler) :: file_handler
  integer :: option
  logical :: running = .true.

  file_handler%clients_db => global_clients
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
        call log_in()
      case (2)
        call sign_in()
      case (3)
        running = .false.
      case default
        print *, "Invalid option"
    end select
  end do  
  print *, "Goodbye!"
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
          print *, "Generating 'clients.dot' file..."
          open(1, file='clients.dot', status='replace')
          write(1, '(A)') "digraph clients {"
          call global_clients%clients_dot(global_clients%root, 1)
          write(1, "(A)") "}"
          close(1)
          print *, "File 'clients.dot' generated!, generating 'clients.svg' file..." 
          call execute_command_line("dot -Tsvg clients.dot -o clients.svg")
          print *, "File 'clients.svg' generated!"
        case (1)
          ! call clients_operations()
        case (2)
          print *, "Be sure to charge the file 'clients.json' in the files folder"
          print *, "Charging..."
          call file_handler%initialize_admin()
          print *, "Charged successfully!"
        case (3)
          ! call reports()
        case (4)
          run = .false.
          print *, "Returning to the main menu..."
        case default
          print *, "Invalid option"
      end select
    end do    
    print *, "See ya!"
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
    call global_clients%add_client(client(trim(username), dpi_num, trim(password)))
    print *, "User added successfully!"
    print *, "-------------------------------------------------"
  end subroutine sign_in
  subroutine log_in()
    implicit none
    character(20) :: dpi_id
    character(20) :: password
    integer*8 :: dpi_num
    integer :: response
    logical :: access = .false.
    do while (.NOT. access)
      print *, "---------------------Log in----------------------"
      print *, "Enter your DPI (Or type r to return):"
      read (*, '(A)') dpi_id  
      if (trim(dpi_id) == "r") then
        return
      end if
      read (dpi_id, '(I13)') dpi_num
      print *, "Enter your password:"
      read (*, '(A)') password
      if ( trim(password) == "diego213" ) then
        write (*,'(A, I13)') "Welcome to Pixel Print Studio, ", dpi_num  
        access = .true.
      else
        print *, "Invalid DPI or password"
      end if
      print *, "-------------------------------------------------"    
    end do
    do while(.TRUE.)
      print *, "-------------------Main Menu---------------------"
      print *, "Select an option:"
      print *, "0. Navigate and manage images"
      print *, "1. Masive charge of images"
      print *, "2. Visual Reports"
      print *, "3. Exit"
      print *, "-------------------------------------------------"
      read (*, *) response
      select case (response)
        case (0)
          ! call navigate_and_manage_images()
        case (1)
          ! call open_images_file()
        case (2)
          ! call visual_reports()
        case (3)
          print *, "Returning to the main menu..."
          return
        case default
          print *, "Invalid option"
      end select      
    end do
    print *, "See ya!"
  end subroutine log_in
end program main
