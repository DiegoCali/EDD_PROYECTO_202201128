module layers
    use pixels
    implicit none        
    type :: layer 
        integer :: id
        integer :: pixels_count
        type(pixel_matrix) :: layer_pixels
        type(layer), pointer :: left => null()
        type(layer), pointer :: right => null()
    end type layer
    type :: layers_tree
        type(layer), pointer :: root => null()
        type(pixel_matrix) :: global_matrix
        integer :: total = 0
    contains
        procedure :: add
        procedure :: add_recursive
        procedure :: add_copied_val
        procedure :: preorder
        procedure :: inorder
        procedure :: postorder
        procedure :: gen_dot
        procedure :: gen_dot_recursive
        procedure :: search
        procedure :: traverse_matrix
        procedure :: max_depth
        procedure :: max_depth_rec
        procedure :: leaf_layers
        procedure :: leaf_layers_rec
        procedure :: list_layers
        procedure :: inorder_print
        procedure :: traverse_limited 
        procedure :: gen_preorder
        procedure :: gen_inorder
        procedure :: gen_postorder
    end type layers_tree
    type node_layer
        type(layer), pointer :: value
        type(node_layer), pointer :: next => null()
    end type node_layer
    type queue
        type(node_layer), pointer :: head => null()
    contains 
        procedure :: enqueue
        procedure :: dequeue
        procedure :: is_empty
    end type queue
contains 
    subroutine gen_inorder(this, tmp, limit, unit)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: tmp
        integer, intent(inout) :: limit
        integer, intent(in) :: unit
        if (.NOT. associated(tmp)) then
            return
        end if
        call this%gen_inorder(tmp%left, limit, unit)
        if (limit == 0) then
            write(unit, '(A)', advance='no') '"];'
            limit = limit - 1
            return
        else if (limit < 0) then
            return
        end if
        write(unit, '(A, I0)') ' layer_', tmp%id
        call tmp%layer_pixels%gen_matrix(this%global_matrix)
        limit = limit - 1
        call this%gen_inorder(tmp%right, limit, unit)
    end subroutine gen_inorder
    subroutine gen_postorder(this, tmp, limit, unit)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: tmp
        integer, intent(inout) :: limit
        integer, intent(in) :: unit
        if (.NOT. associated(tmp)) then
            return
        end if
        call this%gen_postorder(tmp%left, limit, unit)
        call this%gen_postorder(tmp%right, limit, unit)
        if (limit == 0) then
            write(unit, '(A)', advance='no') '"];'
            limit = limit - 1
            return
        else if (limit < 0) then
            return
        end if
        write(unit, '(A, I0)') ' layer_', tmp%id
        call tmp%layer_pixels%gen_matrix(this%global_matrix)
        limit = limit - 1
    end subroutine gen_postorder
    subroutine gen_preorder(this, tmp, limit, unit)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: tmp
        integer, intent(inout) :: limit
        integer, intent(in) :: unit
        if (.NOT. associated(tmp)) then
            return
        end if
        if (limit == 0) then
            write(unit, '(A)', advance='no') '"];'
            limit = limit - 1
        else if (limit < 0) then
            return
        end if
        write(unit, '(A, I0)') ' layer_', tmp%id
        call tmp%layer_pixels%gen_matrix(this%global_matrix)
        limit = limit - 1
        call this%gen_preorder(tmp%left, limit, unit)
        call this%gen_preorder(tmp%right, limit, unit)
    end subroutine gen_preorder
    subroutine traverse_limited(this, order, limit, unit)
        class(layers_tree), intent(inout) :: this
        integer, intent(inout) :: limit
        integer, intent(in) :: unit, order
        write(unit, '(A)', advance='no') 'text_box [shape=box, label="'
        if (order == 0) then
            call this%gen_preorder(this%root, limit, unit)
        else if (order == 1) then
            call this%gen_inorder(this%root, limit, unit)
        else if (order == 2) then
            call this%gen_postorder(this%root, limit, unit)
        else
            print *, 'Invalid option'
        end if
    end subroutine traverse_limited
    subroutine enqueue(this, layer_val)
        class(queue), intent(inout) :: this
        type(layer), pointer, intent(in) :: layer_val
        type(node_layer), pointer :: temp, new_node
        allocate(new_node)
        new_node%value => layer_val
        new_node%next => null()
        if (associated(this%head)) then
            temp => this%head
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => new_node
        else
            this%head => new_node
        end if
    end subroutine enqueue
    function dequeue(this) result(layer_val)
        class(queue), intent(inout) :: this
        type(layer), pointer :: layer_val
        type(node_layer), pointer :: temp
        if (associated(this%head)) then
            layer_val => this%head%value
            temp => this%head
            this%head => this%head%next
        else
            layer_val => null()
        end if
    end function dequeue
    function is_empty(this) result(res)
        class(queue), intent(in) :: this
        logical :: res
        res = .not.associated(this%head)
    end function is_empty
    subroutine traverse_matrix(this)
        class(layers_tree), intent(inout) :: this
        type(queue) :: q
        type(layer), pointer :: tmp
        if (.NOT. associated(this%root)) then
            print *, 'No layers to traverse'
            return
        end if
        call q%enqueue(this%root)
        do while (.NOT. q%is_empty())
            tmp => q%dequeue()
            if (associated(tmp)) then
                write(*, '(A, I0)', advance='no') ' Layer_', tmp%id
                call tmp%layer_pixels%gen_matrix(this%global_matrix)
                if (associated(tmp%left)) then
                    call q%enqueue(tmp%left)
                end if
                if (associated(tmp%right)) then
                    call q%enqueue(tmp%right)
                end if
            end if
        end do
        print *, ''
    end subroutine traverse_matrix
    subroutine add(this,  new_layer)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: new_layer
        if (associated(this%root)) then
            call this%add_recursive(new_layer, this%root)
        else
            this%root => new_layer
        end if 
        this%total = this%total + 1
    end subroutine add
    subroutine add_recursive(this,  new_layer, tmp)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: new_layer
        type(layer), intent(inout) :: tmp
        if ( new_layer%id < tmp%id ) then
            if (associated(tmp%left)) then
                call this%add_recursive(new_layer, tmp%left)
            else
                tmp%left => new_layer
            end if
        else
            if (associated(tmp%right)) then
                call this%add_recursive(new_layer, tmp%right)
            else
                tmp%right => new_layer
            end if            
        end if
    end subroutine add_recursive
    subroutine add_copied_val(this,  new_layer)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: new_layer
        type(layer), pointer :: copied_layer
        if (.NOT. associated(new_layer)) then
            print *, 'Error: new_layer is not associated'
            return
        end if
        allocate(copied_layer)
        copied_layer%id = new_layer%id
        copied_layer%pixels_count = new_layer%pixels_count
        copied_layer%layer_pixels = new_layer%layer_pixels
        if (associated(this%root)) then
            call this%add_recursive(copied_layer, this%root)
        else
            this%root => copied_layer
        end if        
    end subroutine add_copied_val
    function search(this, id_searched) result(tmp)
        class(layers_tree), intent(inout) :: this
        integer, intent(in) :: id_searched
        type(layer), pointer :: tmp
        type(layer), pointer :: current
        current => this%root
        do while (associated(current))
            if ( id_searched < current%id) then
                current => current%left
            end if
            if ( id_searched > current%id ) then
                current => current%right
            end if
            if (current%id == id_searched) then
                tmp => current
                return
            end if
        end do
        tmp => null()  
    end function search
    subroutine preorder(this, tmp)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        if (.NOT. associated(tmp)) then
            return
        end if
        write(*, '(1I3)', advance='no') tmp%id
        call this%preorder(tmp%left)
        call this%preorder(tmp%right)
        ! print *, ''
    end subroutine preorder
    subroutine inorder(this, tmp)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        if (.NOT. associated(tmp)) then
            return
        end if
        call this%inorder(tmp%left)
        write(*, '(I0, A, I0, A)') tmp%id, '. layer (', tmp%pixels_count, ')'
        call this%inorder(tmp%right)   
    end subroutine inorder
    subroutine postorder(this, tmp)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        if (.NOT. associated(tmp)) then
            return
        end if
        call this%postorder(tmp%left)
        call this%postorder(tmp%right)
        write(*, '(1I3)', advance='no') tmp%id  
        ! print *, ''
    end subroutine postorder
    subroutine gen_dot(this, tmp, unit)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        integer, intent(in) :: unit
        write(unit, '(A)') 'digraph layers_tree {'
        call this%gen_dot_recursive(tmp, unit)
        write(unit, '(A)') '}'
    end subroutine gen_dot
    subroutine gen_dot_recursive(this, tmp, unit)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        integer, intent(in) :: unit
        if (.NOT. associated(tmp)) then
            return
        end if
        write(unit, '(A, I0, A, I0, A)') 'node_',tmp%id, ' [label="Capa ', tmp%id, '"];'
        if (associated(tmp%left)) then
            write(unit, '(A, I0, A, I0, A)') 'node_',tmp%id, ' -> node_',tmp%left%id, ';'
        end if
        if (associated(tmp%right)) then
            write(unit, '(A, I0, A, I0, A)') 'node_',tmp%id, ' -> node_',tmp%right%id, ';'
        end if
        call this%gen_dot_recursive(tmp%left, unit)
        call this%gen_dot_recursive(tmp%right, unit)        
    end subroutine gen_dot_recursive
    subroutine max_depth(this)
        class(layers_tree), intent(inout) :: this
        integer :: depth
        depth = 0
        if (.NOT. associated(this%root)) then
            print *, 'No layers to traverse'
            return
        end if  
        depth = this%max_depth_rec(this%root)
        write(*, '(A, I0)') 'Max depth of layers tree: ', depth
    end subroutine max_depth
    recursive function max_depth_rec(this, root) result(depth)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: root
        integer :: depth, left_depth, right_depth
        depth = 0
        if ( .not. associated(root) ) then
            return
        else 
            left_depth = this%max_depth_rec(root%left)
            right_depth = this%max_depth_rec(root%right)
            if (left_depth > right_depth) then
                depth = left_depth + 1
            else
                depth = right_depth + 1
            end if
        end if        
    end function max_depth_rec
    subroutine leaf_layers(this)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer :: tmp
        if (.NOT. associated(this%root)) then
            print *, 'No layers to traverse'
            return
        end if
        call this%leaf_layers_rec(this%root)       
    end subroutine leaf_layers
    subroutine leaf_layers_rec(this, tmp)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: tmp
        if (.NOT. associated(tmp)) then
            return
        end if
        if (.NOT. associated(tmp%left) .AND. .NOT. associated(tmp%right)) then
            write(*, '(I0, A, I0, A)') tmp%id, '. layer (', tmp%pixels_count, ')'
        end if
        call this%leaf_layers_rec(tmp%left)
        call this%leaf_layers_rec(tmp%right)
    end subroutine leaf_layers_rec
    subroutine list_layers(this, option)
        class(layers_tree), intent(inout) :: this
        integer, intent(in) :: option
        if (option == 0) then
            call this%preorder(this%root)
        else if (option == 1) then
            call this%inorder_print(this%root)
        else if (option == 2) then
            call this%postorder(this%root)
        else
            print *, 'Invalid option'
        end if       
        print *, '' 
    end subroutine list_layers
    subroutine inorder_print(this, tmp)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        if (.NOT. associated(tmp)) then
            return
        end if
        call this%inorder_print(tmp%left)
        write(*, '(1I3)', advance='no') tmp%id  
        call this%inorder_print(tmp%right)   
    end subroutine inorder_print
end module layers