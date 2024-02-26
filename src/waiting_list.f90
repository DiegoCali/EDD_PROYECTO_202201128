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
        procedure :: find_client
        procedure :: self_graph
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
        do while (temp%id /= this%head%id)
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
        do while (temp%id /= this%head%id)
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
            write(*, '(a15, a2)', advance='no') current%name, ": "
            call current%images%self_print()
            current => current%next
        else 
            print *, "Waiting list is empty"
        end if
        if (associated(current)) then
            do while (current%id /= this%head%id)
                write(*, '(a15, a2)', advance='no') current%name, ": "
                call current%images%self_print()
                current => current%next
            end do
        end if
    end subroutine print
    subroutine self_graph(this, unit)
        class(wait_list), intent(inout) :: this
        integer, intent(in) :: unit
        character(len=5) :: curr_id, next_id
        character(len=4) :: format_str
        type(client), pointer :: current
        current => this%head
        write(unit, *) "subgraph cluster_2{"
        write(unit, *) "node [style=filled, shape=box];"
        write(unit, *) "rank=same{"
        if (associated(this%head)) then
            if (current%id < 10) then
                format_str = "(I1)"
            else 
                format_str = "(I2)"
            end if
            write(curr_id, format_str) current%id
            write(unit, *) "client_" // curr_id // '[label="' // current%name // '"];'
            if (current%next%id < 10) then
                format_str = "(I1)"
            else 
                format_str = "(I2)"
            end if
            write(next_id, format_str) current%next%id
            write(unit, *) "client_" // curr_id // " -> client_" // next_id // ";"
            current => current%next
            do while (current%id /= this%head%id)
                if (current%id < 10) then
                    format_str = "(I1)"
                else 
                    format_str = "(I2)"
                end if
                write(curr_id, format_str) current%id
                write(unit, *) "client_" // curr_id // '[label="' // current%name // '"];'
                if (current%next%id < 10) then
                    format_str = "(I1)"
                else 
                    format_str = "(I2)"
                end if
                write(next_id, format_str) current%next%id
                write(unit, *) "client_" // curr_id // " -> client_" // next_id // ";"
                current => current%next
            end do
        else 
            write(unit, *) "No_one_waiting;"
        end if            
        write(unit, *) "};"
        write(unit, *) 'label="Lista de espera";'
        write(unit, *) "color=green;"
        write(unit, *) "}"
    end subroutine self_graph
end module waiting_list