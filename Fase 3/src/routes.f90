module routes
    implicit none
    type edge
        integer :: id
        integer :: distance
        integer :: printers
        integer :: weight
        integer :: parent_id
        type(edge), pointer :: next => null()
        type(edge), pointer :: prev => null()
    end type edge
    type edge_list
        type(edge), pointer :: head => null()
        type(edge), pointer :: tail => null()
    contains
        procedure :: add_sorted 
        procedure :: pop
        procedure :: is_empty
        procedure :: merge
        procedure :: add_weight
        procedure :: print_queue
    end type edge_list
    type result
        integer :: id
        integer :: distance 
        integer :: printers
        integer :: weight
        type(result), pointer :: next => null()
    end type result
    type result_list
        integer :: total_weight = 0
        type(result), pointer :: head => null()
        type(result), pointer :: tail => null()
    contains
        procedure :: add_result
        procedure :: print
        procedure :: get_total_weight
    end type result_list
    type node
        integer :: id 
        type(edge_list) :: neighbors
        type(node), pointer :: next => null()
    end type node
    type graph
        integer :: n_nodes
        type(node), pointer :: head => null()
    contains
        procedure :: insert_data
        procedure :: add_node
        procedure :: add_edge
        procedure :: get_node
        procedure :: show_graph
        procedure :: graph_dot
    end type graph
    type analyzer
        type(graph), pointer :: graph_data => null()
    contains
        procedure :: set_graph
        procedure :: get_shortest_path
    end type analyzer
contains
    ! Edge list methods
    subroutine add_sorted(this, id, distance, printers, parent_id, sort_by_id, weight)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: id, distance, printers, parent_id
        integer, intent(inout) :: weight
        logical, intent(in) :: sort_by_id
        type(edge), pointer :: new_edge
        type(edge), pointer :: current
        type(edge), pointer :: previous        
        allocate(new_edge)
        new_edge%id = id
        new_edge%distance = distance
        new_edge%printers = printers
        if (sort_by_id) then
            weight = distance - printers     
            if (weight < 0) then
                weight = 0
            end if   
        end if
        new_edge%weight = weight
        new_edge%parent_id = parent_id

        if (.not. associated(this%head)) then
            this%head => new_edge
            this%tail => new_edge
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight > weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_edge%next => this%head
            this%head%prev => new_edge
            this%head => new_edge
        else if (.not. associated(current)) then
            this%tail%next => new_edge
            new_edge%prev => this%tail
            this%tail => new_edge
        else
            previous%next => new_edge
            new_edge%prev => previous
            new_edge%next => current
            current%prev => new_edge
        end if
    end subroutine add_sorted
    function pop(this) result(edge_res)
        class(edge_list), intent(inout) :: this
        type(edge), pointer :: edge_res
        if (.not. associated(this%head)) then
            edge_res => null()
            return
        end if
        edge_res => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => null()
        else
            this%tail => null()
        end if
    end function pop
    function is_empty(this) result(res)
        class(edge_list), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function is_empty
    subroutine merge(this,  to_merge)
        class(edge_list), intent(inout) :: this
        class(edge_list), intent(in) :: to_merge
        type(edge), pointer :: current

        current => to_merge%head
        do while (associated(current))            
            call this%add_sorted(current%id, current%distance, current%printers, current%parent_id, .FALSE., current%weight)
            current => current%next
        end do
        
    end subroutine merge
    subroutine add_weight(this, weight)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: weight
        type(edge), pointer :: current
        current => this%head
        do while (associated(current))
            current%weight = current%weight + weight            
            current => current%next
        end do        
    end subroutine add_weight
    subroutine print_queue(this)
        class(edge_list), intent(in) :: this
        type(edge), pointer :: current
        current => this%head
        do while (associated(current))
            write(*,'(A, I0, A, I0, A)', advance='no') '(', current%id, ", ", current%weight, '), '
            current => current%next
        end do
        print *, ''
    end subroutine print_queue
    ! Result list methods
    subroutine add_result(this, edge_node)
        class(result_list), intent(inout) :: this
        type(edge), pointer :: edge_node
        type(result), pointer :: new_result, tmp
        allocate(new_result)
        new_result%id = edge_node%id
        new_result%distance = edge_node%distance
        new_result%printers = edge_node%printers
        new_result%weight = edge_node%distance - edge_node%printers
        if (.not. associated(this%head)) then
            this%head => new_result
            this%tail => new_result
            return
        end if
        if ( this%tail%id /= edge_node%parent_id ) then            
            tmp => this%head
            do while (associated(tmp))
                if (tmp%id == edge_node%parent_id) then
                    new_result%next => tmp%next
                    tmp%next => new_result
                    this%tail => new_result                    
                    return
                end if
                tmp => tmp%next
            end do
            return
        else 
            this%tail%next => new_result
            this%tail => new_result
            return
        end if  
    end subroutine add_result
    subroutine print(this)
        class(result_list), intent(inout) :: this
        type(result), pointer :: current
        call this%get_total_weight()
        current => this%head
        do while (associated(current))
            if ( associated(current%next) ) then
                write(*,'(A, I0, A, I0)', advance='no') 'Node: ', current%id, " -> Node: ", current%next%id
                write(*, '(A, I0)') ", Weight: ", current%next%weight
            else 
                write(*,'(A, I0)', advance='no') 'Origin Node: ', this%head%id
                write(*,'(A, I0, A, I0)') ', Final Node: ', current%id, ", Acumulated Weight: ", this%total_weight
            end if
            current => current%next
        end do
    end subroutine print
    subroutine get_total_weight(this)
        class(result_list), intent(inout) :: this
        type(result), pointer :: current
        current => this%head
        do while (associated(current))
            this%total_weight = this%total_weight + current%weight
            current => current%next
        end do
    end subroutine get_total_weight
    ! Graph methods
    subroutine insert_data(this, id, neighbor_id, distance, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, distance, printers
        type(node), pointer :: current

        current => this%get_node(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            current => this%get_node(id)
            call this%add_edge(neighbor_id, distance, printers, current)
        else
            call this%add_edge(neighbor_id, distance, printers, current)
        end if
    end subroutine insert_data
    subroutine add_node(this,  id)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: new_node        
        allocate(new_node)
        new_node%id = id
        
        if (.not. associated(this%head)) then
            this%head => new_node
            return
        end if

        new_node%next => this%head
        this%head => new_node
    end subroutine add_node
    subroutine add_edge(this, id, distance, printers, parent)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, distance, printers
        type(node), pointer :: parent
        type(node), pointer :: edge_node 
        integer :: weight
        weight = 0
        edge_node => this%get_node(id)  
        if ( .NOT. associated(edge_node) ) then            
            call this%add_node(id)
        end if           
        call parent%neighbors%add_sorted(id, distance, printers, parent%id, .TRUE., weight)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_edge
    function get_node(this, id) result(retval)
        class(graph), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: retval
        type(node), pointer :: current
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                retval => current
                return
            end if
            current => current%next
        end do
        retval => null()  
    end function get_node
    subroutine show_graph(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(edge), pointer :: current_edge
        current => this%head
        do while (associated(current))
            write(*,'(A, I0)') 'Node: ', current%id
            current_edge => current%neighbors%head
            do while (associated(current_edge))
                write(*,'(A, I0, A, I0, A)', advance='no') 'Edge: ', current_edge%id, ", " ,current_edge%weight, " "
                current_edge => current_edge%next
            end do
            write(*,*) ''
            current => current%next
        end do
    end subroutine show_graph
    subroutine graph_dot(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(edge), pointer :: current_edge
        open(21, file='outputs/graph.dot', status='replace')
        current => this%head
        write(21, '(A)') 'digraph G {'
        write(21, '(A)') 'rankdir=LR;'
        do while ( associated(current) )
            write(21, '(A, I0)') 'node_', current%id            
            current_edge => current%neighbors%head
            do while ( associated(current_edge) )                
                write(21, '(A, I0, A, I0)', advance='no') 'node_', current%id, ' -> node_', current_edge%id
                write(21, '(A, I0, A)') ' [label="', current_edge%weight, '"];'
                current_edge => current_edge%next
            end do
            current => current%next
        end do
        write(21, '(A)') '}'
        close(21)
        call execute_command_line('dot -Tsvg outputs/graph.dot -o outputs/graph.svg')
    end subroutine graph_dot
    ! Analyzer methods  
    subroutine set_graph(this,  graph_p)
        class(analyzer), intent(inout) :: this
        type(graph), pointer, intent(in) :: graph_p
        this%graph_data => graph_p
    end subroutine set_graph
    function get_shortest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue        
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge, origin_edge
        print *, 'Getting shortest path from ', id_origin, ' to ', id_destination
        sub_total = 0
        allocate(retval)
        retval%total_weight = 0
        allocate(queue)
        allocate(origin_edge)
        origin_edge%id = id_origin
        origin_edge%distance = 0
        origin_edge%printers = 0
        origin_edge%weight = 0
        origin_edge%parent_id = -1
        current_node => this%graph_data%get_node(id_origin)
        if ( associated(current_node) ) then
            call queue%merge(current_node%neighbors)
            call retval%add_result(origin_edge)
        end if
        do while ( .NOT. queue%is_empty() )
            !call queue%print_queue()
            current_edge => queue%pop()            
            sub_total = current_edge%weight            
            current_node => this%graph_data%get_node(current_edge%id)
            if ( .NOT. associated(current_node) ) then
                print *, 'Node not found', current_edge%id
                exit
            end if
            if (current_node%id == id_destination) then
                print *, 'Found destination'
                call retval%add_result(current_edge)
                exit
            end if
            call current_node%neighbors%add_weight(sub_total)            
            call queue%merge(current_node%neighbors)
            call retval%add_result(current_edge)
            current_node => current_node%next
        end do
    end function get_shortest_path
end module routes