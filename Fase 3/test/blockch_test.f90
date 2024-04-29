program blockch_test
    use branch_avl
    use block_chain
    use routes, only: edge, result_list
    implicit none
    type(b_avl), pointer :: tree
    type(branch), pointer :: b    
    type(edge), pointer :: e
    type(result_list), pointer :: rl
    type(block), pointer :: blk
    type(chainer), pointer :: chnr
    ! Creating the blockchain
    allocate(tree)
    allocate(rl)
    allocate(chnr)
    ! Creating branches
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
    ! Creating routes
    allocate(e)
    e%id = 0
    e%distance = 0
    e%printers = 0
    e%weight = 0
    e%parent_id = -1
    call rl%add_result(e)
    allocate(e)
    e%id = 1
    e%distance = 20
    e%printers = 10
    e%weight = 10
    e%parent_id = 0
    call rl%add_result(e)
    allocate(e)
    e%id = 2
    e%distance = 30
    e%printers = 15
    e%weight = 15
    e%parent_id = 1
    call rl%add_result(e)
    allocate(e)
    e%id = 3
    e%distance = 40
    e%printers = 20
    e%weight = 20
    e%parent_id = 2
    call rl%add_result(e)    
    ! Creating blocks
    allocate(blk)
    call blk%generate_block(rl, tree)
    !call blk%print_data()
    call chnr%add_block(blk)
    allocate(blk)
    call blk%generate_block(rl, tree)
    !call blk%print_data()
    call chnr%add_block(blk)
    ! Printing the blockchain
    !call chnr%print_chain()
    call chnr%generate_files()
end program blockch_test