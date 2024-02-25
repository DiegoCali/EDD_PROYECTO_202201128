module printer_queue
    use image_stack
    use client_queue
    use waiting_list
    implicit none
    type printer
        type(image), pointer :: head => null()
        type(wait_list), pointer :: waiting_clients => null()
    contains
        procedure :: push_node
        procedure :: pop 
        procedure :: show_self
        procedure :: execute_step
    end type printer
contains
    subroutine execute_step(this)
        class(printer), intent(inout) :: this
        type(image), pointer :: temp
        type(client), pointer :: searched_client
        type(wait_list), pointer :: clients 
        type(stack), pointer :: client_stack
        clients => this%waiting_clients
        if (associated(this%head)) then
            temp => this%head
            if (temp%size == 0) then
                temp => this%pop()
                searched_client => clients%find_client(temp%client_id)
                client_stack => searched_client%images
                call client_stack%push_node(temp)
            else 
                temp%size = temp%size - 1
            end if
        end if
    end subroutine execute_step
    subroutine push_node(this,  node)
        class(printer), intent(inout) :: this
        type(image), pointer, intent(in) ::  node 
        type(image), pointer :: temp
        node%next => null()
        if (.NOT.associated(this%head)) then
            this%head => node
        else
            temp => this%head
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => node
        end if
    end subroutine push_node   
    function pop(this) result(temp)
        class(printer), intent(inout) :: this
        type(image), pointer :: temp
        if (associated(this%head)) then
            if (this%head%size == 0) then
                temp => this%head
                this%head => this%head%next
            else 
                this%head%size = this%head%size - 1
                temp => null()
            end if
            !print *, "Poped: ", temp%size
        else 
            temp => null()
        end if
    end function pop 
    subroutine show_self(this)
        class(printer), intent(inout) :: this
        type(image), pointer :: current
        current => this%head
        do while (associated(current))
            write (*,"(i2)",advance="no") current%size
            current => current%next
        end do
        print *, ""
    end subroutine show_self
end module printer_queue