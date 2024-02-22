program lists_interaction
    use client_queue
    use image_stack
    use printer_queue
    use window_list
    implicit none
    type(client)  :: first_client
    type(client)  :: second_client
    type(queue)   :: client_queue
    type(stack)   :: first_images
    type(stack)   :: second_images 
    type(windows) :: windows_list
    type(printer) :: g_printer
    type(printer) :: p_printer
    print *, "----------VARIABLES INITIALIZIATION----------"
    call windows_list%create(1)
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
end program lists_interaction