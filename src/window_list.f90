module window_list
    use image_stack
    implicit none
    type :: window
        integer :: id
        type(stack) :: images
        type(window), pointer :: next
        logical :: attending
    end type window
    type :: windows 
        type(window), pointer :: head => null()
    contains
        procedure :: create
        procedure :: search_free_window
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
        current => this%head
        do while (associated(current))
            if (.NOT. current%attending) then
                current%attending = .TRUE.
                stop
            end if
            current => current%next
        end do        
    end function search_free_window
end module window_list