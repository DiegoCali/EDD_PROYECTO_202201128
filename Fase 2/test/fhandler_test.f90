program file_test
    use filehandler
    use clients
    implicit none
    type(Btree_clients), target :: global_clients
    type(client), pointer :: actual_client
    type(fhandler) :: file_handler
    file_handler%clients_db => global_clients
    call file_handler%initialize_admin()
    actual_client => global_clients%search_client(global_clients%root, 2897315340401_8)
    if (associated(actual_client)) then
        print *, 'Client found'
        print *, actual_client%name
        call file_handler%set_user(actual_client)
        call file_handler%initialize_user()
        call actual_client%all_images%print_images(actual_client%all_images%root)
        print *, ''
        call actual_client%all_layers%inorder(actual_client%all_layers%root)
        print *, ''
        call actual_client%list_albums%show_albums()
    else
        print *, 'Client not found'
    end if
end program file_test