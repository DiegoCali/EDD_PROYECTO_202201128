program avl_test
    use branch_avl
    implicit none
    type(b_avl), pointer :: tree
    type(branch), pointer :: b
    allocate(tree)

    allocate(b)
    b%key = 0
    b%place = "San Pedro Sacatepequez"
    b%address = "Zona 3"
    b%password = "das4648sdf"
    call tree%add_branch(b)
    allocate(b)
    b%key = 1
    b%place = "Ciudad de Guatemala"
    b%address = "Zona 1"
    b%password = "das4648sdf"
    call tree%add_branch(b)
    allocate(b)
    b%key = 2
    b%place = "Chimaltenango"
    b%address = "Zona 2"
    b%password = "das4648sdf"
    call tree%add_branch(b)
    allocate(b)
    b%key = 3
    b%place = "San Pedro Sacatepequez"
    b%address = "Zona 3"
    b%password = "das4648sdf"
    call tree%add_branch(b)
    !call tree%search_branch(2)
    call tree%get_dot()
    
end program avl_test