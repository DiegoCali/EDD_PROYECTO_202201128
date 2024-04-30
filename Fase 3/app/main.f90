program main
    use tech_hash
    use routes
    use branch_avl
    use block_chain
    use file_handler
    use sha256_module
    implicit none
    type(b_avl), pointer :: branches
    type(branch), pointer :: current_branch
    type(hash), pointer :: tech_table
    type(fhandler) :: fhand_helper
    type(graph), pointer :: general_graph
    type(analyzer) :: graph_analyzer
    type(result_list), pointer :: results
    type(chainer), pointer :: block_chain_gen 
    type(block), pointer :: new_block
    character(len=100) :: user, password

    allocate(branches)
    allocate(block_chain_gen)
    current_branch => null()
    general_graph => null()
    call fhand_helper%connect(branches)

    do while (.TRUE.)
        print *, "||========== Enter your username: ==========||"
        read (*,'(A)') user
        print *, "||========== Enter your password: ==========||"
        read (*,'(A)') password
        if (trim(user) == "EDD1S2024" .and. trim(password) == "ProyectoFase3") then
            print *, "||========== Welcome to the system ==========||"
            call login()
            exit
        else
            print *, "||========== Invalid username or password ==========||"
            print *, "Try again..."
        end if
    end do
    print *, "||==========End of the program==========||"
contains
    subroutine login()
        implicit none
        integer :: option
        logical :: go_out
        go_out = .FALSE.
        do while (.NOT. go_out)
            print *, "||========== Choose an option: ==========||"
            print *, "1. Load files"
            print *, "2. Branches"
            print *, "3. Reports"
            print *, "4. Exit"
            read (*,*) option
            select case(option)
                case(1)
                    call load_files()                    
                case(2)
                    call view_branches()                    
                case(3)
                    !call reports()
                    print *, "Not implemented yet..."                    
                case(4)                    
                    go_out = .TRUE.
                case default
                    print *, "||==========Invalid option==========||"
            end select
        end do
    end subroutine login
    subroutine load_files()
        implicit none
        character(len=100) :: file_name
        integer :: option
        logical :: go_out
        go_out = .FALSE.
        do while (.NOT. go_out)
            print *, "||========== Choose an option: ==========||"
            print *, "1. Load branches"
            print *, "2. Load a graph"
            print *, "3. Exit"
            read (*,*) option
            select case(option)
                case(1)
                    print *, "||========== Enter the file name: ==========||"
                    read (*,'(A)') file_name
                    call fhand_helper%read_branches(file_name)
                    call branches%print_self()    
                    call branches%get_dot()                                           
                case(2)
                    print *, "||========== Enter the file name: ==========||"
                    read (*,'(A)') file_name
                    general_graph => fhand_helper%read_graph(file_name)
                    if (associated(general_graph)) then
                        print *, "||========== Graph loaded successfully ==========||"
                        call general_graph%show_graph()
                        call general_graph%graph_dot()
                    else
                        print *, "||========== Error loading the graph ==========||"
                    end if                                        
                case(3)
                    go_out = .TRUE.
                case default
                    print *, "||==========Invalid option==========||"
            end select
        end do
    end subroutine load_files
    subroutine view_branches()
        implicit none 
        integer :: id_branch
        character(len=100) :: password_branch
        print *, "||========== Enter the branch id: ==========||"
        read (*,*) id_branch
        current_branch => branches%search_branch(id_branch)
        if (associated(current_branch)) then
            print *, "||========== Enter the password: ==========||"
            read (*,'(A)') password_branch
            password_branch = sha256(password_branch)
            if (trim(password_branch) == current_branch%password) then
                print *, "||========== Branch found ==========||"
                call branch_options()
            else
                print *, "||========== Invalid password ==========||"
            end if
        else
            print *, "||========== Branch not found ==========||"
        end if
    end subroutine view_branches
    subroutine branch_options()
        implicit none
        character(len=100) :: file_name
        integer :: option, node1, node2
        integer*8 :: tech_id
        logical :: go_out
        go_out = .FALSE.
        do while (.NOT. go_out)
            print *, "||========== Choose an option: ==========||"
            print *, "1. Load technicians"
            print *, "2. Generate shortest path"
            print *, "3. Info of a technician"
            print *, "4. Show technicians"
            print *, "5. Reports"
            print *, "6. Exit"
            read (*,*) option
            select case(option)
                case(1)
                    print *, "||========== Enter the file name: ==========||"
                    read (*,'(A)') file_name
                    call fhand_helper%read_techs(file_name, current_branch%key)
                    call current_branch%hash_table%hash_dot()
                    print *, "||========== Technicians loaded successfully ==========||"
                case(2)
                    if (associated(general_graph)) then
                        call graph_analyzer%set_graph(general_graph)
                        print *, "||========== Enter the node 1: ==========||"
                        read (*,*) node1
                        print *, "||========== Enter the node 2: ==========||"
                        read (*,*) node2
                        results => graph_analyzer%get_shortest_path(node1, node2)
                        call results%print()
                        allocate(new_block)
                        call new_block%generate_block(results, branches)
                        call block_chain_gen%add_block(new_block)
                        call block_chain_gen%generate_files()
                    else
                        print *, "||========== Graph not loaded ==========||"
                    end if
                case(3)
                    print *, "||========== Enter the technician id: ==========||"
                    call current_branch%hash_table%show()
                    read (*,*) tech_id
                    call current_branch%hash_table%search(tech_id)
                case(4)
                    call current_branch%hash_table%show()
                case(5)
                    !call reports()
                    print *, "Not implemented yet..."
                case(6)
                    go_out = .TRUE.
                case default
                    print *, "||==========Invalid option==========||"
            end select
        end do        
    end subroutine branch_options
end program main