module image_stack
    implicit none
    private
    type, public :: image
        integer :: size
        character(len=:), allocatable :: belongs_to
        type(image), pointer :: next
    end type image
    type, public :: stack
        type(image), pointer :: head => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: self_print
        procedure :: push_node
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
    end subroutine push_node
    function pop(this) result(temp)
        class(stack), intent(inout) :: this
        type(image), pointer :: temp
        temp => this%head
        this%head => this%head%next
        !print *, "Poped: ", temp%size
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