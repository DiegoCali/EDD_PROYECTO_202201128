module window_list
    use client_queue
    use image_stack
    use waiting_list
    use printer_queue
    implicit none
    type :: window
        integer :: id
        type(stack) :: images
        type(window), pointer :: next
        type(client), pointer :: client
        logical :: attending
    end type window
    type :: windows 
        type(window), pointer :: head => null()
        type(wait_list), pointer :: waiting_queue => null()
        type(printer), pointer :: big_printer => null()
        type(printer), pointer :: small_printer => null()
    contains
        procedure :: create
        procedure :: search_free_window
        procedure :: print_attending
        procedure :: get_images
        procedure :: self_print
        procedure :: opened_windows
        procedure :: send_images
        procedure :: graph_self
    end type windows
contains
    subroutine create(this, id)
        class(windows), intent(inout) :: this
        integer, intent(in) :: id
        type(window), pointer :: temp
        allocate(temp)
        temp%id = id
        temp%attending = .FALSE.
        temp%next => null()
        if (.NOT. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if
    end subroutine create
    function search_free_window(this) result(current)
        class(windows), intent(inout) :: this
        type(window), pointer :: current  
        logical :: running      
        running = .TRUE.
        current => this%head
        do while (associated(current).AND.running)
            if (.NOT.current%attending) then
                current%attending = .TRUE.
                running = .FALSE.
            else 
                current => current%next
            end if
        end do   
    end function search_free_window
    subroutine print_attending(this)
        class(windows), intent(inout) :: this
        type(window), pointer :: current
        current => this%head
        do while(associated(current))            
            if (current%attending) then
                write(*, '(A8,i1,A14)', advance='no') "Window:", current%id, "Is attending:"
                print *, current%client%name
            end if
            current => current%next
        end do        
    end subroutine print_attending
    subroutine self_print(this)
        class(windows), intent(inout) :: this
        type(window), pointer :: current
        current => this%head
        do while (associated(current))
            print *, "window:", current%id
            call current%images%self_print()
            current => current%next
        end do
    end subroutine self_print
    subroutine get_images(this)
        class(windows), intent(inout) :: this
        type(window), pointer :: current
        current => this%head
        do while (associated(current))            
            if (current%attending)then
                if (.NOT.current%client%images%empty) then
                    call current%images%push_node(current%client%images%pop())
                else 
                    current%attending = .FALSE.
                    current%client%being_attended = .FALSE.
                    call this%send_images(current)
                    call this%waiting_queue%add(current%client)
                end if
            end if
            current => current%next
        end do
    end subroutine get_images
    function opened_windows(this) result(response)
        class(windows), intent(inout) :: this
        type(window), pointer :: current
        logical :: response
        response = .FALSE.
        current => this%head
        do while (associated(current))
            if (.NOT.current%attending) then 
                response = .TRUE.
            end if
            current => current%next
        end do
    end function opened_windows
    subroutine send_images(this, window_sending)
        class(windows), intent(inout) :: this
        type(window), pointer :: window_sending
        type(stack), pointer :: stack_to_send
        type(image), pointer :: temp
        stack_to_send => window_sending%images
        do while (associated(stack_to_send%head))
            temp => stack_to_send%pop()
            if (temp%size == 2) then
                call this%big_printer%push_node(temp)
            else 
                call this%small_printer%push_node(temp)
            end if
        end do
    end subroutine send_images
    subroutine graph_self(this, unit)
        class(windows), intent(inout) :: this
        integer, intent(in) :: unit
        character(len=5) :: curr_id, next_id
        character(len=4) :: format_str
        type(window), pointer :: current
        type(image), pointer :: temp_img
        current => this%head
        write(unit, *) "subgraph cluster_1{"
        write(unit, *) "node [style=filled, shape=box];"
        do while (associated(current))
            ! to connect with client if needed
            if (current%id < 10) then
                format_str = "(I1)"
            else 
                format_str = "(I2)"
            end if
            write(next_id, format_str) current%id
            ! If has images proceds to make the conections
            if (current%attending) then
                if (current%client%id < 10) then
                    format_str = "(I1)"
                else 
                    format_str = "(I2)"
                end if
                write(curr_id, format_str) current%client%id
                write(unit, *) "client_" // curr_id // '[label="' // current%client%name // '"];'
                write(unit, *) "rank=same{"
                write(unit, *) "client_" // curr_id // " -> ventanilla_" // next_id
                if (associated(current%images%head)) then
                    write(unit, "(A)", advance="no") "-> "
                    temp_img => current%images%head
                    do while(associated(temp_img))
                        if (temp_img%id < 10) then
                            format_str = "(I1)"
                        else 
                            format_str = "(I2)"
                        end if
                        write(curr_id, format_str) temp_img%id
                        write(unit, *) "img_" // curr_id
                        if (associated(temp_img%next)) then
                            write(unit, '(A)', advance="no") " -> "
                        else 
                            write(unit, *) "};"
                        end if
                        temp_img => temp_img%next
                    end do
                else 
                    write(unit, *) "};"
                end if
            end if
            ! Connects to the following window 
            if (current%id < 10) then
                format_str = "(I1)"
            else 
                format_str = "(I2)"
            end if
            write(curr_id, format_str) current%id
            if (associated(current%next)) then
                if (current%next%id < 10) then
                    format_str = "(I1)"
                else 
                    format_str = "(I2)"
                end if
                write(next_id, format_str) current%next%id
                write(unit, *) "ventanilla_" // curr_id // " -> " // "ventanilla_" // next_id // ";"
            end if
            current => current%next
        end do
        write(unit, *) 'label="Sala de espera";'
        write(unit, *) "color=blue;"
        write(unit, *) "}"
    end subroutine graph_self
end module window_list