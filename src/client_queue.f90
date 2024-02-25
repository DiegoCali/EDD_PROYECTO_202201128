module client_queue
    use image_stack
    implicit none
    private
    type, public :: client
        character(:), allocatable :: name
        integer :: id
        type(stack) :: images
        type(client), pointer :: next
        type(client), pointer :: prev
        integer :: steps
        integer :: g_images
        integer :: p_images
        logical :: waiting
        logical :: being_attended
        logical :: finished
    contains
        procedure :: own_images
    end type client
    type, public :: queue
        type(client), pointer :: head => null()
        integer :: num_clients
    contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: self_print
        procedure :: check
        procedure :: start
    end type queue
contains
    subroutine start(this, num_clients)
        class(queue), intent(inout) :: this
        integer, intent(in) :: num_clients
        this%num_clients = num_clients
    end subroutine
    subroutine enqueue(this, name, client_image_stack)
        class(queue), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(stack), intent(in) :: client_image_stack
        type(client), pointer :: new_client
        type(client), pointer :: temp
        allocate(new_client)
        new_client%id = this%num_clients
        new_client%name = name                      ! initialize client name
        new_client%images = client_image_stack      ! set image stack
        call new_client%own_images()                ! make all images in stack have client id
        new_client%steps = 0                        ! to calculate the number of steps in the future
        new_client%g_images = 0                     ! to save the quatity of "imagenes grandes"
        new_client%p_images = 0                     ! to save the quantity of "imagenes pequenias"
        new_client%being_attended = .FALSE.         ! boolean to check if has a window
        new_client%finished = .FALSE.               ! boolean to check if process finished
        new_client%next => null()                   ! next client in his list
        new_client%prev => null()                   ! prev client in double linked list 
        if (.NOT.associated(this%head)) then
            this%head => new_client            
        else 
            temp => this%head
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => new_client
        end if
        this%num_clients = this%num_clients + 1
    end subroutine enqueue
    function dequeue(this) result(temp)
        class(queue), intent(inout) :: this
        type(client), pointer :: temp
        if (associated(this%head)) then        
            temp => this%head
            this%head => temp%next
        else 
            temp => null()
        end if
    end function dequeue
    function check(this) result(temp)
        class(queue), intent(inout) :: this
        type(client), pointer :: temp
        logical :: running
        running = .TRUE.
        if (associated(this%head)) then
            temp => this%head
        else 
            temp => null()
            return
        end if
        do while (associated(temp).AND.running)
            if (.NOT.temp%being_attended) then
                temp%being_attended = .TRUE.
                temp => this%dequeue()
                running = .FALSE.
            else 
                temp => temp%next
            end if 
        end do
    end function check
    subroutine self_print(this)
        class(queue), intent(inout) :: this
        type(client), pointer :: current        
        if (associated(this%head)) then
            current => this%head
        else 
            print *, "No more clients in queue"
            return
        end if        
        do while (associated(current))
            write (*,"(a15, a)",advance="no") current%name, ":"
            call current%images%self_print()
            current => current%next
        end do
    end subroutine self_print
    subroutine own_images(this)
        class(client), intent(inout) :: this
        class(image), pointer :: current
        current => this%images%head
        do while (associated(current))
            current%client_id = this%id
            current => current%next
        end do
    end subroutine own_images
end module client_queue