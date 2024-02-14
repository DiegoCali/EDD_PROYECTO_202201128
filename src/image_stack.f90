module image_stack
    implicit none
    private
    type, public :: image
        character(len=1) :: size
        type(image), pointer :: next
    end type image
    type, public :: stack
        type(image), pointer :: head => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: self_print
    end type stack
contains
    subroutine push(this, size)
        class(stack), intent(inout) :: this
        character(len=1), intent(in) :: size
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
        print *, "Pushed: ", size
    end subroutine push
    subroutine pop(this)
        class(stack), intent(inout) :: this
        type(image), pointer :: temp
        temp => this%head
        this%head => this%head%next
        print *, "Poped: ", temp%size
        deallocate(temp)
    end subroutine pop
    subroutine self_print(this)
        class(stack), intent(inout) :: this
        type(image), pointer :: current
        current => this%head
        do while (associated(current))
            print *, current%size
            current => current%next
        end do
    end subroutine self_print
end module image_stack