module images
    use layers
    implicit none
    type :: image
        integer :: id
        type(layers_tree) :: layers
    end type image
    type :: image_avl
        type(image), pointer :: root => null()
    end type image_avl
contains
    
end module images