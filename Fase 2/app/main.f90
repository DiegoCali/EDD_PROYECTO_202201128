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
          open(1, file='outputs/clients.dot', status='replace')
          write(1, '(A)') "digraph clients {"
          call global_clients%clients_dot(global_clients%root, 1)
          write(1, "(A)") "}"
          close(1)
          print *, "File 'clients.dot' generated!, generating 'clients.svg' file..." 
          call execute_command_line("dot -Tsvg outputs/clients.dot -o outputs/clients.svg")
          print *, "File 'clients.svg' generated!"
        case (1)
          call clients_operations()
        case (2)
          print *, "Be sure to charge the file 'clients.json' in the 'files' folder"
          print *, "Charging..."
          call file_handler%initialize_admin()
          print *, "Charged successfully!"
        case (3)
          ! call admin_reports()
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
    type(client), pointer :: temp
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
      temp => global_clients%search_client(global_clients%root, dpi_num)
      if ( associated(temp) ) then
        if ( trim(password) == temp%password ) then
          write (*,'(A, I13, A, A)') "Welcome to Pixel Print Studio, ", dpi_num, ": ", temp%name
          access = .true.
          actual_client => temp
        else
          print *, "Invalid password"
        end if
      else 
        print *, "Invalid DPI or password, could not find the user in the database..."
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
          call images_operations()
        case (1)
          print *, "Be sure to charge the files 'layers.json', 'images.json' and 'albums.json' in the 'files' folder"
          print *, "Setting client..."
          call file_handler%set_user(actual_client)
          print *, "Charging files..."
          call file_handler%initialize_user()
          print *, "Charged successfully!"
        case (2)
          ! call visual_reports()
        case (3)
          print *, "Returning to the main menu..."
          return
        case default
          print *, "Invalid option"
      end select      
    end do
    print *, "-------------------------------------------------"
  end subroutine log_in
  subroutine images_operations()
    implicit none
    integer :: op
    logical :: run = .true.
    do while (run)
      print *, "-----------------Images operations----------------"
      print *, "Select an option:"
      print *, "0. Insert new image"
      print *, "1. Delete image"
      print *, "2. Navigate all images"
      print *, "3. Navigate by album"
      print *, "4. Exit"
      print *, "-------------------------------------------------"
      read *, op
      select case (op)
        case (0)
          call insert_image()
        case (1)
          call delete_image()
        case (2)
          call navigate_images()
        case (3)
          call navigate_albums()
        case (4)
          run = .false.
          print *, "Returning to the main menu..."
        case default
          print *, "Invalid option"
      end select
    end do
    print *, "-------------------------------------------------"
  end subroutine images_operations
  subroutine delete_image()
    implicit none
    integer :: image_id
    type(image), pointer :: temp_img
    character(1) :: response
    print *, "-----------------Delete image-------------------"
    call actual_client%all_images%print_images(actual_client%all_images%root)
    print *, "Enter the id of the image to delete:"
    read (*, *) image_id
    temp_img => actual_client%all_images%search_img(actual_client%all_images%root, image_id)
    if ( associated(temp_img) ) then
      print *, "Image found!"
      print *, "Do you wish to delete the image? [y/n]"
      read (*, '(A)') response
      if (response == "y") then
        call actual_client%all_images%delete_img(image_id)
        print *, "Image deleted successfully!"
      end if
    else
      print *, "Image not found..."
    end if
    print *, "-------------------------------------------------"
  end subroutine delete_image
  subroutine navigate_albums()  
    use albums, only: album
    implicit none
    integer :: album_id, op
    type(album), pointer :: temp_album
    character(1) :: response
    logical :: run = .true.
    do while (run)
      print *, "-----------------Navigate albums-----------------"
      print *, "Please select any of the following albums:"
      call actual_client%list_albums%show_albums()
      print *, "Enter the id of the album:"
      read (*, *) album_id
      temp_album => actual_client%list_albums%get_album(album_id)
      if ( associated(temp_album) ) then
        call temp_album%show_images()
        print *, "Do you wish to exit? [y/n]"
        read (*, '(A)') response
        if (response == "y") then
          run = .false.
        end if
      else
        print *, "Album not found... try again!"
      end if
    end do
    print *, "-------------------------------------------------"
  end subroutine navigate_albums
  subroutine navigate_images()
    implicit none
    integer :: image_id, op, layer_id
    type(layer), pointer :: temp_layer
    type(image), pointer :: temp_img
    logical :: run = .true.
    do while (run)
      print *, "-----------------Navigate images-----------------"
      print *, "Please select any of the following images:"
      call actual_client%all_images%print_images(actual_client%all_images%root)
      print *, "Enter the id of the image:"
      read (*, *) image_id
      temp_img => actual_client%all_images%search_img(actual_client%all_images%root, image_id)
      if ( associated(temp_img) ) then
        print *, "Image found!"
        print *, "-------------------------------------------------"
        print *, "Select an option:"
        print *, "0. Generate image"
        print *, "1. Generate layer matrix"
        print *, "2. Exit"
        read (*, *) op
        select case (op)
          case (0)
            open(1, file='outputs/image.dot', status='replace')
            write(1, '(A)') "digraph image {"
            call temp_img%layers%traverse_matrix()
            call temp_img%layers%global_matrix%global_m_dot(1)
            write(1, "(A)") "}"
            close(1)
            print *, "File 'image.dot' generated!, generating 'image.svg' file..."
            call execute_command_line("dot -Tsvg outputs/image.dot -o outputs/image.svg")
            print *, "File 'image.svg' generated!"
          case (1)
            print *, "Select layer to graph:"
            call temp_img%layers%inorder(temp_img%layers%root)
            read (*, *) layer_id
            temp_layer => temp_img%layers%search(layer_id)  
            if ( associated(temp_layer) ) then
              print *, "Generating 'single_layer.dot' file..."
              open(1, file='outputs/single_layer.dot', status='replace')
              write(1, '(A)') "digraph single_layer {"
              call temp_layer%layer_pixels%graph_pixels(1)
              write(1, "(A)") "}"
              close(1)
              print *, "File 'single_layer.dot' generated!, generating 'single_layer.svg' file..."
              call execute_command_line("dot -Tsvg outputs/single_layer.dot -o outputs/single_layer.svg")
              print *, "File 'single_layer.svg' generated!"
            else
              print *, "Layer not found..."
            end if            
          case (2)
            print *, "Returning to the images menu..."
            run = .false.
        end select
        print *, "-------------------------------------------------"
      else
        print *, "Image not found... try again!"
      end if
    end do
    print *, "-------------------------------------------------"
  end subroutine navigate_images
  subroutine insert_image()
    implicit none
    integer :: id_img, id_layer
    type(image), pointer :: temp_img
    type(layer), pointer :: temp_layer
    type(client), pointer :: temp
    character(1) :: response
    logical :: finished_layers = .false.
    print *, "-----------------Insert image-------------------"
    call actual_client%all_images%print_images(actual_client%all_images%root)
    print *, "Enter an id for the new image [Do not repeat id's]:"
    read (*, *) id_img
    allocate(temp_img)
    temp_img%id = id_img
    do while (.not. finished_layers)
      call actual_client%all_layers%inorder(actual_client%all_layers%root)
      print *, "Enter the id of the layer [Do not repeat id's, type -1 to terminate]:"
      read (*, *) id_layer
      if (id_layer == -1) then
        finished_layers = .true.
      else
        temp_layer => actual_client%all_layers%search(id_layer)
        call temp_img%layers%add_copied_val(temp_layer)
      end if
    end do
    call actual_client%all_images%add_img(temp_img)
    print *, "Image added successfully!"
    print *, "Do you wish to generate the image? [y/n]"
    read (*, '(A)') response
    if (response == "y") then
      open(1, file='outputs/image.dot', status='replace')
      write(1, '(A)') "digraph image {"
      call temp_img%layers%traverse_matrix()
      call temp_img%layers%global_matrix%global_m_dot(1)
      write(1, "(A)") "}"
      close(1)
      print *, "File 'image.dot' generated!, generating 'image.svg' file..."
      call execute_command_line("dot -Tsvg outputs/image.dot -o outputs/image.svg")
      print *, "File 'image.svg' generated!"
    end if
    print *, "-------------------------------------------------"
  end subroutine insert_image
  subroutine clients_operations()
    implicit none
    integer :: op
    character(20) :: username, password, dpi_str
    type(client), pointer :: temp
    integer*8 :: dpi_num
    logical :: run = .true.
    do while (run)
      print *, "-----------------Users operations----------------"
      print *, "Select an option:"
      print *, "0. Insert user"
      print *, "1. Remove user"
      print *, "2. Modify user"
      print *, "3. Exit"
      print *, "-------------------------------------------------"
      read *, op
      select case (op)
        case (0)
          print *, "Enter the username:"
          read (*, '(A)') username
          print *, "Enter the password:"
          read (*, '(A)') password
          print *, "Enter the DPI:"
          read (*, '(A)') dpi_str
          read (dpi_str, '(I13)') dpi_num
          call global_clients%add_client(client(trim(username), dpi_num, trim(password)))
        case (1)
          print *, "Enter the DPI of the user to remove:"
          read (*, '(A)') dpi_str
          read (dpi_str, '(I13)') dpi_num
          call global_clients%delete_client(dpi_num)
          print *, "User removed successfully!"
        case (2)
          print *, "Enter the DPI of the user to modify:"
          read (*, '(A)') dpi_str
          read (dpi_str, '(I13)') dpi_num
          print *, "Enter the new username:"
          read (*, '(A)') username
          print *, "Enter the new password:"
          read (*, '(A)') password
          temp => global_clients%search_client(global_clients%root, dpi_num)
          if ( associated(temp) ) then
            temp%name = trim(username)
            temp%password = trim(password)
            print *, "User modified successfully!"
          else
            print *, "User not found..."
          end if
        case (3)
          run = .false.
          print *, "Returning to the admin menu..."
        case default
          print *, "Invalid option"
      end select
    end do
    print *, "-------------------------------------------------"
  end subroutine clients_operations
end program main
