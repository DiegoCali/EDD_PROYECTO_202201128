module window_list
    use image_stack
    use client_queue
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
    contains
        procedure :: create
        procedure :: search_free_window
        procedure :: print_attending
        procedure :: get_images
        procedure :: self_print
        procedure :: opened_windows
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
        class(window), pointer :: current
        current => this%head
        do while(associated(current))               
            if (current%attending) then
                print *, "Window:", current%id, "Is attending:", current%client%name
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
        class(window), pointer :: current
        current => this%head
        do while (associated(current))
            if (current%attending)then
                if (.NOT.current%client%images%empty) then
                    call current%images%push_node(current%client%images%pop())
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
end module window_list