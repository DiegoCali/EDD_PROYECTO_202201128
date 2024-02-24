program lists_interaction
    use client_queue
    use image_stack
    use printer_queue
    use window_list
    use waiting_list
    implicit none
    type(client)            :: first_client
    type(client)            :: second_client
    type(queue)             :: client_queue
    type(stack)             :: first_images
    type(stack)             :: second_images 
    type(windows)           :: windows_list
    type(wait_list), target :: clients_waiting
    type(printer), target   :: g_printer
    type(printer), target   :: p_printer
    type(window), pointer   :: temp_window
    print *, "----------VARIABLES INITIALIZIATION----------"
    ! Obligated initialization: *
    call client_queue%start(0)   ! *                 
    call windows_list%create(1)  ! * made with a cycle
    call windows_list%create(2)
    call first_images%push(2)
    call first_images%push(2)
    call first_images%push(1)
    call second_images%push(2)
    call second_images%push(1)
    call second_images%push(1)
    call client_queue%enqueue("Diego", first_images)
    call client_queue%enqueue("Pablo", second_images)
    call client_queue%self_print()
    windows_list%waiting_queue => clients_waiting ! linking the window list and waiting list  *
    windows_list%big_printer => g_printer         ! linking the window list and big printer   *
    windows_list%small_printer => p_printer       ! linking the window list and small printer * 
    print *, "------------STEP 1-------------"
    call windows_list%get_images()
    if (associated(client_queue%head)) then
        if (windows_list%opened_windows()) then
            temp_window => windows_list%search_free_window()    
            temp_window%client => client_queue%check()
        end if
    end if
    call windows_list%print_attending()
    print *, "client queue:"
    call client_queue%self_print()
    print *, "waiting list:"
    call clients_waiting%print()
    print *, "window info:"
    call windows_list%self_print()
    print *, "big printer:"
    call g_printer%show_self()
    print *, "small printer:"
    call p_printer%show_self()
    print *, "------------STEP 2-------------"
    call windows_list%get_images()
    if (associated(client_queue%head)) then
        if (windows_list%opened_windows()) then
            temp_window => windows_list%search_free_window()    
            temp_window%client => client_queue%check()
        end if
    end if
    call windows_list%print_attending()
    print *, "client queue:"
    call client_queue%self_print()
    print *, "waiting list:"
    call clients_waiting%print()
    print *, "window info:"
    call windows_list%self_print()
    print *, "big printer:"
    call g_printer%show_self()
    print *, "small printer:"
    call p_printer%show_self()
    print *, "------------STEP 3-------------"
    call windows_list%get_images()
    if (associated(client_queue%head)) then
        if (windows_list%opened_windows()) then
            temp_window => windows_list%search_free_window()    
            temp_window%client => client_queue%check()
        end if
    end if
    call windows_list%print_attending()
    print *, "client queue:"
    call client_queue%self_print()
    print *, "waiting list:"
    call clients_waiting%print()
    print *, "window info:"
    call windows_list%self_print()
    print *, "big printer:"
    call g_printer%show_self()
    print *, "small printer:"
    call p_printer%show_self()
    print *, "------------STEP 4-------------"
    call windows_list%get_images()
    if (associated(client_queue%head)) then
        if (windows_list%opened_windows()) then
            temp_window => windows_list%search_free_window()    
            temp_window%client => client_queue%check()
        end if
    end if
    call windows_list%print_attending()
    print *, "client queue:"
    call client_queue%self_print()
    print *, "waiting list:"
    call clients_waiting%print()
    print *, "window info:"
    call windows_list%self_print()
    print *, "big printer:"
    call g_printer%show_self()
    print *, "small printer:"
    call p_printer%show_self()
    print *, "------------STEP 5-------------"
    call windows_list%get_images()
    if (associated(client_queue%head)) then
        if (windows_list%opened_windows()) then
            temp_window => windows_list%search_free_window()    
            temp_window%client => client_queue%check()
        end if
    end if
    call windows_list%print_attending()
    print *, "client queue:"
    call client_queue%self_print()
    print *, "waiting list:"
    call clients_waiting%print()
    print *, "window info:"
    call windows_list%self_print()
    print *, "big printer:"
    call g_printer%show_self()
    print *, "small printer:"
    call p_printer%show_self()
end program lists_interaction