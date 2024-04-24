program doc_test
    use file_handler
    use tech_hash
    use branch_avl
    implicit none
    type(hash), pointer :: table
    type(b_avl), pointer :: branch_data_base
    type(fhandler) :: fh
    character(len=100) :: file_route, file_route2

    allocate(table)
    allocate(branch_data_base)

    call fh%connect(table, branch_data_base)

    call table%init(7, 0, 70)
    
    print  *, 'Enter the file route for hash table:'
    read(*, '(A)') file_route

    call fh%read_techs(file_route)

    call table%show()

    call table%hash_dot()
    
    print *, 'Enter the file route for AVL tree:'
    read(*, '(A)') file_route2

    call fh%read_branches(file_route2)

    call branch_data_base%print_self()

    call branch_data_base%get_dot()

end program doc_test