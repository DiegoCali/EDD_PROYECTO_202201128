program client_test
    use clients
    implicit none
    type(Btree_clients) :: clients_db
    type(client) :: new_client

    new_client = client("John", 14_8, "password")
    call clients_db%add_client(new_client)
    new_client = client("Jane", 66_8, "password")
    call clients_db%add_client(new_client)
    new_client = client("Doesy", 167_8, "password")
    call clients_db%add_client(new_client)
    new_client = client("Johny", 31_8, "password")
    call clients_db%add_client(new_client)
    new_client = client("Donnie", 22_8, "password")
    call clients_db%add_client(new_client)
    new_client = client("Kathy", 34_8, "password")
    call clients_db%add_client(new_client)
    new_client = client("Lucy", 41_8, "password")
    call clients_db%add_client(new_client)
    call clients_db%traversal(clients_db%root)
    print *, ""
end program client_test