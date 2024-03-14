module layers
    use pixels
    implicit none        
    type :: layer 
        integer :: id
        type(pixel_matrix) :: layer_pixels
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
        type(layer), intent(in) :: new_layer
        
    
        
    end subroutine add
    subroutine add_recursive(this,  new_layer)
        class(layers_tree), intent(inout) :: this
        type(layer), intent(in) :: new_layer
        
    
        
    end subroutine add_recursive
    subroutine preorder(this)
        class(layers_tree), intent(inout) :: this        
        
    
        
    end subroutine preorder
    subroutine inorder(this)
        class(layers_tree), intent(inout) :: this        
        
    
        
    end subroutine inorder
    subroutine postorder(this)
        class(layers_tree), intent(inout) :: this        
        
    
        
    end subroutine postorder
    subroutine gen_dot(this)
        class(layers_tree), intent(inout) :: this        
        
    
        
    end subroutine gen_dot
    subroutine gen_dot_recursive(this)
        class(layers_tree), intent(inout) :: this        
        
    
        
    end subroutine gen_dot_recursive
end module layers