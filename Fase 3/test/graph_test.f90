program graph_test
    use routes
    implicit none
    type(graph), pointer :: g
    type(analyzer) :: a
    type(result_list), pointer :: r

    allocate(g)
    print *, "Creating graph..."
    call g%insert_data(1, 2, 10, 5)
    call g%insert_data(1, 3, 12, 6)
    call g%insert_data(2, 3, 12, 6)
    call g%insert_data(2, 4, 6, 3)
    call g%insert_data(3, 4, 10, 5)
    call g%insert_data(3, 5, 4, 2)
    call g%insert_data(4, 5, 7, 4)
    call g%insert_data(4, 6, 8, 4)
    call g%insert_data(5, 6, 2, 1)
    print *, "Graph created."
    call g%graph_dot()
    call a%set_graph(g)
    r => a%get_shortest_path(1, 6)
    call r%print()
end program graph_test