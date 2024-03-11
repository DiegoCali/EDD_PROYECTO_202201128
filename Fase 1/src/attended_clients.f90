module attended_clients
	use client_queue
	implicit none
	integer :: file_unit = 9
	type attended_list
		type(client), pointer :: head => null()
		type(client), pointer :: tail => null()
	contains
		procedure :: push_node
		procedure :: consult
		procedure :: search_id
		procedure :: search_most_step
		procedure :: graph_attended
		procedure :: sort
		procedure :: graph_sort
	end type attended_list
contains
	subroutine push_node(this,  node)
	        class(attended_list), intent(inout) :: this
	        type(client), pointer, intent(in) ::  node 
	        node%next => null()
	        node%prev => null()
	        if (.NOT.associated(this%head)) then
	            this%head => node
	            this%tail => node
	        else
	            node%prev => this%tail
		    this%tail%next => node	
		    this%tail => node	    
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
				open(file_unit, file="consult.dot", status="replace")
				write(file_unit, *) "graph H{"
				write(file_unit, *) "node [style=filled, shape=box]"
				write(file_unit, *) 'response [label="'
				write(file_unit, '(A,I3)') "ID: ", current%id
				write(file_unit, '(A,A)') "Name: ", current%name
				write(file_unit, '(A,I3)') "g_images: ", current%g_images
				write(file_unit, '(A,I3)') "p_images: ", current%p_images
				write(file_unit, '(A,I3)') "Steps: ", current%steps
				write(file_unit, *) '"];'
				write(file_unit, *) "}"
				close(file_unit)
				call execute_command_line('dot -Tsvg consult.dot > result.svg')
			        call execute_command_line('eog result.svg')
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
			open(file_unit, file="consult.dot", status="replace")
			write(file_unit, *) "graph H{"
			write(file_unit, *) "node [style=filled, shape=box]"
			write(file_unit, *) 'response [label="'
			write(file_unit, '(A,I3)') "ID: ", temp%id
			write(file_unit, '(A,A)') "Name: ", temp%name
			write(file_unit, '(A,I3)') "g_images: ", temp%g_images
			write(file_unit, '(A,I3)') "p_images: ", temp%p_images
			write(file_unit, '(A,I3)') "Steps: ", temp%steps
			write(file_unit, *) '"];'
			write(file_unit, *) "}"
			close(file_unit)
			call execute_command_line('dot -Tsvg consult.dot > result.svg')
		        call execute_command_line('eog result.svg')
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
	subroutine sort(this, big_image)
		class(attended_list), intent(inout) :: this
		logical, intent(in) :: big_image
		type(client), pointer :: current, forward
		character(:), allocatable :: temp_name
		integer :: temp_num_images
		
		if (.NOT.associated(this%head)) then
			print *, "No hay clientes atendidos"
		end if
		
		current => this%head
		do while (associated(current))
			forward => current%next
			do while (associated(forward))
				if (big_image) then
					if (current%g_images < forward%g_images) then
						temp_name = current%name
						temp_num_images = current%g_images
						current%name = forward%name
						current%g_images = forward%g_images
						forward%name = temp_name
						forward%g_images = temp_num_images											
					end if
				else
					if (current%p_images < forward%p_images) then
						temp_name = current%name
						temp_num_images = current%p_images
						current%name = forward%name
						current%p_images = forward%p_images
						forward%name = temp_name
						forward%p_images = temp_num_images				
					end if
				end if
				forward => forward%next
			end do
			current => current%next
		end do
		call this%graph_sort()
	end subroutine sort
	subroutine graph_sort(this)
		class(attended_list), intent(inout) :: this
	        character(len=5) :: next_id
	        character(len=5) :: curr_id 
	        character(len=4) :: format_str
	        type(client), pointer :: current
	        integer :: i
	        current => this%head
	        open(file_unit, file="consult.dot", status="replace")
	        write(file_unit, *) "digraph H{"
	        write(file_unit, *) "node [style=filled, shape=box];"
	        if (.NOT.associated(this%head))then
	            write(file_unit, *) "No_clients_attended;"
	        end if
	        i = 1
	        do while (associated(current).AND. (i<5))
	            if (current%id < 10) then
	                format_str = "(I1)"
	            else 
	                format_str = "(I2)"
	            end if
	            write(curr_id, format_str) current%id
	            write(file_unit, *) "client_" // curr_id // '[label ="' // current%name // '"];'
	            if (associated(current%next)) then
	                if (current%next%id < 10) then
	                    format_str = "(I1)"
	                else 
	                    format_str = "(I2)"
	                end if
	                write(next_id, format_str) current%next%id
	                write(file_unit, *)  "client_" // curr_id // " -> client_" // next_id // ";"
	            end if    
	            current => current%next
	            i = i + 1
	        end do
	        write(file_unit, *) "}"
	        close(file_unit)
		call execute_command_line('dot -Tsvg consult.dot > result.svg')
	        call execute_command_line('eog result.svg')
	end subroutine graph_sort
end module attended_clients
