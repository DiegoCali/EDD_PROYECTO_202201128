module waiting_list
    use client_queue
    implicit none
    type :: wait_list 
        type(client), pointer :: head => null()
        type(client), pointer :: tail => null()
    contains
        procedure :: add
        procedure :: check
    end type wait_list
contains
    subroutine add(this, client_node)
        class(wait_list), intent(inout) :: this
        type(client), pointer, intent(in) :: client_node
        client_node%next => null()
        client_node%prev => null()
        if (.NOT. associated(this%head)) then
            this%head => client_node
            this%tail => client_node
            client_node%next => client_node
            client_node%prev => client_node
        else 
            this%tail%next => client_node
            this%head%prev => client_node
            client_node%next => this%head
            client_node%prev => this%tail
            this%tail => client_node
        end if
    end  subroutine add
    function check(this) result(temp)
        class(wait_list), intent(inout) :: this
        type(client), pointer :: temp
        ! Nothing yet        
    end function check
end module waiting_list