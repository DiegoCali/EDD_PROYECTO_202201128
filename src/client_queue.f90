module client_queue
    use image_stack
    implicit none
    private
    type, public :: client
        character(len=20) :: name
        type(stack) :: images
        type(client), pointer :: next
    end type client
    type, public :: queue
        type(client), pointer :: head => null()
    contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: self_print
    end type queue
contains
    subroutine enqueue(this, name, client_image_stack)
        class(queue), intent(inout) :: this
        character(len=20), intent(in) :: name
        type(stack), intent(in) :: client_image_stack
        type(client), pointer :: new_client
        type(client), pointer :: temp
        allocate(new_client)
        new_client%name = name
        new_client%images = client_image_stack
        new_client => null()
        temp => this%head
        if (associated(temp)) then
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => new_client
        else 
            this%head => new_client
        end if
    end subroutine enqueue
    subroutine dequeue(this)
        class(queue), intent(inout) :: this
    end subroutine dequeue
    subroutine self_print(this)
        class(queue), intent(inout) :: this
    end subroutine self_print
end module client_queue