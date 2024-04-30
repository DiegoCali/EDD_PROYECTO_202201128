module file_handler
    use json_module
    use json_kinds, only: ck
    use tech_hash
    use branch_avl
    use routes
    use sha256_module
    implicit none
    type, public :: fhandler            
        type(b_avl), pointer :: branches_tree => null() ! branches avl tree
    contains
        procedure :: connect 
        procedure :: read_techs  
        procedure :: read_branches  
        procedure :: read_graph
    end type fhandler
contains
    subroutine connect(self, branch_avl_tree)
        class(fhandler), intent(inout) :: self
        type(hash), pointer :: tech_hash_table
        type(b_avl), pointer :: branch_avl_tree
        
        self%branches_tree => branch_avl_tree    
        
        print *, 'Connection successful!'        
    end subroutine connect
    subroutine read_techs(self, file_route, branch_id)   
        class(fhandler), intent(inout) :: self  
        character(len=*), intent(in) :: file_route        
        integer, intent(in) :: branch_id
        type(hash), pointer :: tech_hash_table
        type(branch), pointer :: branch_searched
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: list_p, techs_p, attr_p
        character(kind=ck, len=:), allocatable :: dpi 
        integer*8 :: dpi_value
        character(kind=ck, len=:), allocatable :: name, last_name, address, gender, cell
        integer :: i, size
        logical :: found

        allocate(tech_hash_table)
        call tech_hash_table%init(7, 0, 70)
        
        call json%initialize()
        call json%load(filename=trim(file_route))
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)

        do i = 1, size
            call jsonc%get_child(list_p, i, techs_p, found=found)
            call jsonc%get_child(techs_p, 'dpi', attr_p, found=found)
            
            if ( found ) then
                call jsonc%get(attr_p, dpi)
                read(dpi, '(I13)') dpi_value
            end if

            call jsonc%get_child(techs_p, 'nombre', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, name)
            end if

            call jsonc%get_child(techs_p, 'apellido', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, last_name)
            end if

            call jsonc%get_child(techs_p, 'genero', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, gender)
            end if

            call jsonc%get_child(techs_p, 'direccion', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, address)
            end if

            call jsonc%get_child(techs_p, 'telefono', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, cell)
            end if

            call tech_hash_table%insert(tech(dpi_value, name, last_name, address, gender, cell))
        end do
        call json%destroy()
        branch_searched => self%branches_tree%search_branch(branch_id)
        if ( associated(branch_searched) ) then
            branch_searched%hash_table => tech_hash_table
        end if
    end subroutine read_techs
    subroutine read_branches(this, file_route)
        class(fhandler), intent(inout) :: this
        character(len=*), intent(in) :: file_route
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: list_p, branch_p, attr_p
        type(branch), pointer :: new_branch
        character(kind=ck, len=:), allocatable :: place, address, password 
        integer :: i, size, key 
        logical :: found

        call json%initialize()
        call json%load(filename=trim(file_route))
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)

        do i = 1, size
            call jsonc%get_child(list_p, i, branch_p, found=found)
            call jsonc%get_child(branch_p, 'id', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, key)
            end if

            call jsonc%get_child(branch_p, 'departamento', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, place)
            end if

            call jsonc%get_child(branch_p, 'direccion', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, address)
            end if

            call jsonc%get_child(branch_p, 'password', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, password)
            end if

            allocate(new_branch)
            new_branch%key = key
            new_branch%place = place
            new_branch%address = address
            new_branch%password = sha256(password)
            call this%branches_tree%add_branch(new_branch)
        end do
    end subroutine read_branches
    function read_graph(this, file_route) result(graph_read)
        class(fhandler), intent(in) :: this
        character(len=*), intent(in) :: file_route
        type(graph), pointer :: graph_read
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: list_p, graph_p, edge_p, attr_p
        integer :: i, size, node_o, node_d, distance, printers 
        logical :: found    
        
        call json%initialize()
        call json%load(filename=trim(file_route))
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)

        allocate(graph_read)

        call jsonc%get_child(list_p, 'grafo', graph_p, found=found)
        call json%info('grafo', n_children=size)

        do i = 1, size
            call jsonc%get_child(graph_p, i, edge_p, found=found)
            call jsonc%get_child(edge_p, 's1', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, node_o)
            end if
            call jsonc%get_child(edge_p, 's2', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, node_d)
            end if
            call jsonc%get_child(edge_p, 'distancia', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, distance)
            end if
            call jsonc%get_child(edge_p, 'imp_mantenimiento', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, printers)
            end if
            call graph_read%insert_data(node_o, node_d, distance, printers)
        end do

        call json%destroy()        
    end function read_graph
end module file_handler