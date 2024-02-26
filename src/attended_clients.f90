module attended_clients
	use client_queue
	implicit none
	type attended_list
		type(client), pointer :: head => null()
	contains
		procedure :: push_node
		procedure :: consult
		procedure :: search_id
		procedure :: search_most_step
		procedure :: graph_attended
	end type attended_list
contains
	subroutine push_node(this,  node)
	        class(attended_list), intent(inout) :: this
	        type(client), pointer, intent(in) ::  node 
	        node%next => null()
	        node%prev => null()
	        if (.NOT.associated(this%head)) then
	            this%head => node
	        else
	            node%next => this%head
	            this%head => node
	        end if
	end subroutine push_node
	subroutine consult(this)
		class(attended_list), intent(inout) :: this
		class(client), pointer :: current
		integer :: response
		current => this%head
		if (associated(current)) then
			print *, "Select the id of the client:"
			do while (associated(current))
				write(*, '(A,A,I3)') current%name, ": ", current%id
				current => current%next
			end do
			read *, response
			call this%search_id(response)
		else
			print *, "No hay clientes atendidos"
		end if
	end subroutine
	subroutine search_id(this, id)
		class(attended_list), intent(inout) :: this
		integer, intent(in) :: id
		class(client), pointer :: current
		current => this%head
		do while (associated(current))
			if (current%id == id) then
				write(*, '(A,I3,A)', advance='no') "Cliente: ", current%id, current%name
				write(*, '(A,I3,I3,I3)') " ", current%g_images, current%p_images, current%steps
				return
			end if
			current => current%next
		end do
		print *, "No se encontró el cliente"
	end subroutine search_id
	subroutine search_most_step(this)
		class(attended_list), intent(inout) :: this
		class(client), pointer :: current
		class(client), pointer :: temp
		current => this%head
		temp => this%head
		do while (associated(current))
			if (temp%steps < current%steps) then
				temp => current
			end if
			current => current%next
		end do
		if (associated(temp)) then
			write(*, '(A,A)') "Cliente con mas pasos: ", temp%name
		end if
	end subroutine search_most_step
	subroutine graph_attended(this, unit)
       		class(attended_list), intent(inout) :: this
	        integer, intent(in) :: unit
	        character(len=5) :: next_id
	        character(len=5) :: curr_id 
	        character(len=4) :: format_str
	        type(client), pointer :: current
	        current => this%head
	        write(unit, *) "subgraph cluster_4{"
	        write(unit, *) "node [style=filled, shape=box];"
	        if (.NOT.associated(this%head))then
	            write(unit, *) "No_clients_attended;"
	        end if
	        do while (associated(current))
	            if (current%id < 10) then
	                format_str = "(I1)"
	            else 
	                format_str = "(I2)"
	            end if
	            write(curr_id, format_str) current%id
	            write(unit, *) "client_" // curr_id // '[label ="' // current%name // '"];'
	            if (associated(current%next)) then
	                if (current%next%id < 10) then
	                    format_str = "(I1)"
	                else 
	                    format_str = "(I2)"
	                end if
	                write(next_id, format_str) current%next%id
	                write(unit, *)  "client_" // curr_id // " -> client_" // next_id // ";"
	            end if    
	            current => current%next
	        end do
	        write(unit, *) 'label="Clientes Atendidos";'
	        write(unit, *) "color=red;"
	        write(unit, *) "}"
	end subroutine graph_attended
end module attended_clients
