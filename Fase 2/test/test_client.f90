program client_test
    use clients
    use json_module
    implicit none 
    type(Btree_clients) :: clients_db
    type(client), pointer :: client_p, new_client
    type(json_file) :: json
    type(json_value), pointer :: list_p, preson_p, attr_p
    type(json_core) :: jsonc 
    character(:), allocatable :: dpi_str, name_str, pass_str
    integer :: i, j, size 
    integer*8 :: dpi
    logical :: found 
    
    call json%initialize()
    call json%load(filename="files/clients.json")

    call json%info('', n_children=size)

    call json%get_core(jsonc)
    call json%get('', list_p, found)

    do i = 1, size
        call jsonc%get_child(list_p, i, preson_p, found=found)
        call jsonc%get_child(preson_p, "dpi", attr_p, found=found)
        if (found) then
            call jsonc%get(attr_p, dpi_str)
            read(dpi_str, '(I13)') dpi
        end if
        call jsonc%get_child(preson_p, "nombre_cliente", attr_p, found=found)
        if (found) then
            call jsonc%get(attr_p, name_str)
        end if
        call jsonc%get_child(preson_p, "password", attr_p, found=found)
        if (found) then
            call jsonc%get(attr_p, pass_str)
        end if
        call clients_db%add_client(client(name_str, dpi, pass_str))
    end do

    client_p => clients_db%search_client(clients_db%root, 2897315340401_8)
    if ( associated(client_p) ) then
        print *, 'Found client:', client_p%name
    end if
    call clients_db%delete_client(1231231231231_8)
    open(1, file="outputs/clients.dot", status="replace")
    write(1, "(A)") "digraph clients {"
    call clients_db%clients_dot(clients_db%root, 1)
    write(1, "(A)") "}"
    close(1)
    call execute_command_line("dot -Tsvg outputs/clients.dot > outputs/clients.svg")
end program client_test