module routes
    implicit none
    type node
        integer :: id 
        integer :: cost
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(node_list), pointer :: neighbors
    end type node
    type node_list
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains         
    end type node_list    
    type routes_analyzer
        type(node_list), pointer :: nodes
    end type routes_analyzer
contains
    
end module routes