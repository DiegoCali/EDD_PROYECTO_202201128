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
    integer                 :: step
    character(len=1)        :: response
    logical                 :: running
    print *, "----------VARIABLES INITIALIZIATION----------"
    ! Obligated initialization: *
    running = .TRUE.             ! *
    step = 0
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
    g_printer%waiting_clients => clients_waiting  ! linking big printer and waiting clients   *
    p_printer%waiting_clients => clients_waiting  ! linking small printer and waiting clients *
    do while (running)
        print *, "execute another step? [y/n]"
        read *, response
        if (response == 'y') then
            write(*, '(a20,i2,a15)') "//-------------STEP:", step, "-------------\\"
            call g_printer%execute_step()
            call p_printer%execute_step()
            call windows_list%get_images()
            if (associated(client_queue%head)) then
                if (windows_list%opened_windows()) then
                    temp_window => windows_list%search_free_window()    
                    temp_window%client => client_queue%check()
                end if
            end if
            print *, "---------WINDOW STATUS----------"
            call windows_list%print_attending()
            print *, "----------CLIENT QUEUE----------"
            call client_queue%self_print()
            print *, "----------WAITING LIST----------"
            call clients_waiting%print()
            print *, "----------WINDOW INFO:----------"
            call windows_list%self_print()
            print *, "----------BIG PRINTER:----------"
            call g_printer%show_self()
            print *, "---------SMALL PRINTER----------"
            call p_printer%show_self()
            print *, "--------------END---------------"
        else 
            running = .FAlSE.
        end if
        step = step + 1
    end do
end program lists_interaction