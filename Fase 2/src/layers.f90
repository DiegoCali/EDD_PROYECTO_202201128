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
    contains
        procedure :: add
        procedure :: add_recursive
        procedure :: preorder
        procedure :: inorder
        procedure :: postorder
        procedure :: gen_dot
        procedure :: gen_dot_recursive
    end type layers_tree
contains 
    subroutine add(this,  new_layer)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: new_layer
        if (associated(this%root)) then
            call this%add_recursive(new_layer, this%root)
        else
            this%root => new_layer
        end if 
    end subroutine add
    subroutine add_recursive(this,  new_layer, tmp)
        class(layers_tree), intent(inout) :: this
        type(layer), pointer, intent(in) :: new_layer
        type(layer), intent(inout) :: tmp
        if ( new_layer%pixels_count < tmp%pixels_count ) then
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
        write(*, '(1I3)', advance='no') tmp%id
        call this%inorder(tmp%right)   
        ! print *, ''
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
    subroutine gen_dot(this, tmp)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp

        open(2, file='layers_tree.dot', status='replace')
        write(2, '(A)') 'digraph G {'
        call this%gen_dot_recursive(tmp)
        write(2, '(A)') '}'
        
    end subroutine gen_dot
    subroutine gen_dot_recursive(this, tmp)
        class(layers_tree), intent(inout) :: this        
        type(layer), pointer, intent(in) :: tmp
        if (.NOT. associated(tmp)) then
            return
        end if
        write(2, '(A, I0, A, I0, A)') 'node_',tmp%id, ' [label="Capa ', tmp%id, '"];'
        if (associated(tmp%left)) then
            write(2, '(A, I0, A, I0, A)') 'node_',tmp%id, ' -> node_',tmp%left%id, ';'
        end if
        if (associated(tmp%right)) then
            write(2, '(A, I0, A, I0, A)') 'node_',tmp%id, ' -> node_',tmp%right%id, ';'
        end if
        call this%gen_dot_recursive(tmp%left)
        call this%gen_dot_recursive(tmp%right)        
    end subroutine gen_dot_recursive
end module layers