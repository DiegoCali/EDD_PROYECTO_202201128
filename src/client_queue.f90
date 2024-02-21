module client_queue
    use image_stack
    use window_list
    implicit none
    private
    type, public :: client
        character(:), allocatable :: name
        integer :: id
        type(stack) :: images
        type(client), pointer :: next
        type(window), pointer :: attendant
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
        character(len=*), intent(in) :: name
        type(stack), intent(in) :: client_image_stack
        type(client), pointer :: new_client
        type(client), pointer :: temp
        allocate(new_client)
        new_client%name = name
        new_client%images = client_image_stack
        new_client%next => null()        
        if (.NOT.associated(this%head)) then
            this%head => new_client            
        else 
            temp => this%head
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => new_client
        end if
    end subroutine enqueue
    subroutine dequeue(this)
        class(queue), intent(inout) :: this
        type(client), pointer :: temp
        temp => this%head
        this%head => temp%next
        deallocate(temp)
    end subroutine dequeue
    subroutine self_print(this)
        class(queue), intent(inout) :: this
        type(client), pointer :: current        
        current => this%head
        do while (associated(current))
            write (*,"(a15, a)",advance="no") current%name, ":"
            call current%images%self_print
            current => current%next
        end do
    end subroutine self_print
end module client_queue