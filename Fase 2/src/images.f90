module images
    use layers
    implicit none
    type :: image
        integer :: id
        integer :: height
        type(layers_tree) :: layers
    end type image
    type :: image_avl
        type(image), pointer :: root => null()
    contains
        procedure :: add_img
        procedure :: add_img_rec
        procedure :: srl
        procedure :: srr
        procedure :: drl
        procedure :: drr
        procedure :: get_max
    end type image_avl
contains
    subroutine add_img(this,  new_image)
        class(image_avl), intent(inout) :: this
        type(image), intent(in) :: new_image
    
        
    end subroutine add_img
    subroutine add_img_rec(this,  new_image, tmp)
        class(image_avl), intent(inout) :: this
        type(image), intent(in) :: new_image
        type(image), pointer :: tmp
    
        
    end subroutine add_img_rec
    function srl(this, t1) result(t2)
        class(image_avl), intent(inout) :: this
        type(image), pointer, intent(in) :: t1
        type(image), pointer :: t2
    
        
    end function srl
    function srr(this, t1) result(t2)
        class(image_avl), intent(inout) :: this
        type(image), pointer, intent(in) :: t1
        type(image), pointer :: t2
    
        
    end function srr
    function drl(this, tmp) result(res)
        class(image_avl), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        type(image), pointer :: res
           
        
    end function drl
    function drr(this, tmp) result(res)
        class(image_avl), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        type(image), pointer :: res
           
        
    end function drr
    function get_max(this, val1, val2) result(res)
        class(image_avl), intent(in) :: this
        integer, intent(in) :: val1, val2 
        integer :: res
        res = merge(val1, val2, val1 > val2)   
    end function get_max
end module images