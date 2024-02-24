module waiting_list
    use client_queue
    implicit none
    type :: wait_list 
        type(client), pointer :: head => null()
        type(client), pointer :: tail => null()
    contains
        procedure :: add
        procedure :: check
        procedure :: print
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
    function find_client(this, client_id) result(temp)
        class(wait_list), intent(inout) :: this
        integer, intent(in) :: client_id
        type(client), pointer :: temp
        temp => null()
        if (.NOT. associated(this%head)) then
            return
        end if
        temp => this%head
        if (temp%id == client_id) then
            return
        end if
        temp => this%head%next
        do while (temp%name /= this%head%name)
            if (temp%id == client_id) then
                return
            end if
            temp => temp%next
        end do
    end function find_client
    function check(this) result(temp)
        class(wait_list), intent(inout) :: this
        type(client), pointer :: temp
        temp => null()
        if (.NOT. associated(this%head)) then
            return
        end if
        temp => this%head
        if (temp%finished) then
            return
        end if
        temp => this%head%next
        do while (temp%name /= this%head%name)
            if (temp%finished) then
                return
            end if
            temp => temp%next
        end do
        ! Nothing yet        
    end function check
    subroutine print(this)
        class(wait_list), intent(inout) :: this
        type(client), pointer :: current    
        if (associated(this%head)) then
            current => this%head
            print *, current%name
            current => current%next
        else 
            print *, "Waiting list is empty"
        end if
        if (associated(current)) then
            do while (current%name /= this%head%name)
                print *, current%name
                current => current%next
            end do
        end if
    end subroutine print
end module waiting_list