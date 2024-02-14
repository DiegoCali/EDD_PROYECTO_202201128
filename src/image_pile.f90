module image_pile
    implicit none
    private
    type, public :: node
        character :: size
        type(node), pointer :: next
    end type node
    type, public :: stack
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: self_print
    end type stack
contains
    subroutine push(this, size)
        class(stack), intent(inout) :: this
        character, intent(in) :: size
        type(node), pointer :: temp
        allocate(temp) 
        temp%size = size
        if (.NOT. associated(this%head)) then
            this%head => temp
            temp%next => null()
        else
            temp%next => this%head
            this%head => temp%next
        end if
    end subroutine push
    subroutine pop(this)
        class(stack), intent(inout) :: this
    end subroutine pop
    subroutine self_print(this)
        class(stack), intent(inout) :: this
    end subroutine self_print
end module image_pile