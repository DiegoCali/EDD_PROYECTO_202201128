program graph_test
    use routes
    implicit none
    type(graph), pointer :: g
    type(analyzer) :: a
    type(result_list), pointer :: r

    allocate(g)
    print *, "Creating graph..."
    call g%insert_data(1, 2, 5)
    call g%insert_data(1, 3, 6)
    call g%insert_data(2, 3, 6)
    call g%insert_data(2, 4, 3)
    call g%insert_data(3, 4, 5)
    call g%insert_data(3, 5, 2)
    call g%insert_data(4, 5, 3)
    call g%insert_data(4, 6, 4)
    call g%insert_data(5, 6, 1)
    print *, "Graph created."
    !call g%show()
    call a%set_graph(g)
    r => a%get_shortest_path(1, 6)
    call r%print()
end program graph_test