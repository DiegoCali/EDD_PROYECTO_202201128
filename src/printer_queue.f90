module printer_queue
    use image_stack
    implicit none
    type printer
        type(image), pointer :: head => null()
    contains
        procedure :: push_node
        procedure :: pop 
    end type printer
contains
    subroutine push_node(this,  node)
        class(printer), intent(inout) :: this
        type(image), pointer, intent(in) ::  node 
        type(image), pointer :: temp
        node%next => null()
        if (.NOT.associated(this%head)) then
            this%head => node
        else
            temp => this%head
            do while (associated(temp))
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
end module printer_queue