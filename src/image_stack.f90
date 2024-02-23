module image_stack
    implicit none
    private
    type, public :: image
        integer :: size
        integer :: client_id
        type(image), pointer :: next
    end type image
    type, public :: stack
        type(image), pointer :: head => null()     
        logical :: empty   
    contains
        procedure :: push
        procedure :: pop
        procedure :: push_node
        procedure :: self_print
    end type stack
contains
    subroutine push(this, size)
        class(stack), intent(inout) :: this
        integer, intent(in) :: size
        type(image), pointer :: temp
        allocate(temp) 
        temp%size = size
        temp%next => null()
        if (.NOT.associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp            
        end if
        this%empty = .FALSE.
        !print *, "Pushed: ", size
    end subroutine push
    subroutine push_node(this,  node)
        class(stack), intent(inout) :: this
        type(image), pointer, intent(in) ::  node 
        node%next => null()
        if (.NOT.associated(this%head)) then
            this%head => node
        else
            node%next => this%head
            this%head => node
        end if
        this%empty = .FALSE.
    end subroutine push_node
    function pop(this) result(temp)
        class(stack), intent(inout) :: this
        type(image), pointer :: temp
        if (associated(this%head)) then
            temp => this%head
            this%head => this%head%next
            !print *, "Poped: ", temp%size
            if (.NOT. associated(this%head)) then
                this%empty = .TRUE.
            end if 
        else
            temp => null()
        end if
    end function pop
    subroutine self_print(this)
        class(stack), intent(inout) :: this
        type(image), pointer :: current
        current => this%head
        do while (associated(current))
            write (*,"(i2)",advance="no") current%size
            current => current%next
        end do
        print *, ""
    end subroutine self_print
end module image_stack