program doc_test
    use file_handler
    use tech_hash
    use branch_avl
    use routes
    implicit none
    type(hash), pointer :: table
    type(b_avl), pointer :: branch_data_base
    type(branch), pointer :: branch_data
    type(graph), pointer :: g
    type(fhandler) :: fh
    character(len=100) :: file_route, file_route2, file_route3

    allocate(table)
    allocate(branch_data_base)
    
    call fh%connect(branch_data_base)        
    
    print *, 'Enter the file route for AVL tree:'
    read(*, '(A)') file_route2

    call fh%read_branches(file_route2)

    call branch_data_base%print_self()

    !call branch_data_base%get_dot()

    print  *, 'Enter the file route for hash table:'
    read(*, '(A)') file_route

    call fh%read_techs(file_route, 2)

    branch_data => branch_data_base%search_branch(2)
    call branch_data%hash_table%show()

    !call table%hash_dot()

    print *, 'Enter the file route for Graph:'
    read(*, '(A)') file_route3

    g => fh%read_graph(file_route3)

    call g%show()

end program doc_test