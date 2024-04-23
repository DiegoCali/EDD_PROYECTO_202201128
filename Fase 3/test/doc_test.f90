program doc_test
    use file_handler
    use tech_hash
    implicit none
    type(hash), pointer :: table
    type(fhandler) :: fh
    character(len=100) :: file_route

    allocate(table)

    call fh%connect(table)

    call table%init(7, 0, 70)
    
    read(*, '(A)') file_route

    call fh%read_techs(file_route)

    call table%show()
    
end program doc_test