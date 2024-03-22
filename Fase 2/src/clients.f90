module clients
    use images, only: image_avl
    use albums, only: album_list
    implicit none
    integer :: g_id = 1
    type :: client 
        character(:), allocatable :: name
        integer*8 :: dpi 
        character(:), allocatable :: password
        type(image_avl) :: all_images
        type(album_list) :: list_albums
    end type client
    type :: nodeptr
        type(BtreeNode), pointer :: ptr => null()
    end type nodeptr
    type :: BtreeNode
        integer :: id
        type(client) :: clients(0:5)
        integer :: num = 0
        type(nodeptr) :: links(0:5)
    end type BtreeNode
    type :: Btree_clients
        type(BtreeNode), pointer :: root => null()
    contains
        procedure :: add_client
        procedure :: create_node
        procedure :: traversal
        procedure :: clients_dot
    end type Btree_clients
contains
    subroutine add_client(this,  new_client)
        class(Btree_clients), intent(inout) :: this
        type(client), intent(in) :: new_client
        type(client) :: i
        type(BtreeNode), pointer :: child 
        allocate(child)
        child%id = g_id
        g_id = g_id + 1
        if ( set_value(new_client, i, this%root, child) ) then
            this%root => this%create_node(i, child)
        end if    
    end subroutine add_client
    recursive function set_value( new_client, pclient,  node, child) result(retval)
        type(client), intent(in) :: new_client
        type(client), intent(inout) :: pclient
        type(BtreeNode), pointer, intent(inout) :: node
        type(BtreeNode), pointer, intent(inout) :: child
        type(BtreeNode), pointer :: new_node
        integer :: pos
        logical :: retval
        allocate(new_node)
        new_node%id = g_id
        g_id = g_id + 1
        if ( .NOT. associated(node) ) then            
            pclient = new_client
            child => null()
            retval = .true.
            return
        end if
        if ( new_client%dpi < node%clients(1)%dpi ) then
            pos = 0
        else 
            pos = node%num
            do while ( new_client%dpi < node%clients(pos)%dpi .AND. pos > 1 )
                pos = pos - 1
            end do
            if ( new_client%dpi == node%clients(pos)%dpi ) then
                print *, "Client already exists, no duplicates allowed"
                retval = .false.
                return
            end if
        end if
        if ( set_value(new_client, pclient, node%links(pos)%ptr, child) ) then
            if ( node%num < 4 ) then
                call insert_node(pclient, pos, node, child)
            else
                call split_node(pclient, pclient, pos, node, child, new_node)
                child => new_node
                retval = .true.
                return
            end if
        end if
        retval = .false.
    end function set_value
    function create_node(this, new_client, child) result(retval)
        class(Btree_clients), intent(inout) :: this
        type(client), intent(in) :: new_client
        type(BtreeNode), pointer, intent(in) :: child
        type(BtreeNode), pointer :: retval
        integer :: i
        allocate(retval)
        retval%clients(1) = new_client
        retval%num = 1
        retval%id = g_id
        g_id = g_id + 1
        retval%links(0)%ptr => this%root
        retval%links(1)%ptr => child
        do i = 2, 4
            retval%links(i)%ptr => null()
        end do
    end function create_node
    subroutine insert_node(pclient, pos, node, child)
        type(client), intent(in) :: pclient
        integer, intent(in) :: pos
        type(BtreeNode), pointer, intent(inout) :: node
        type(BtreeNode), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while ( j > pos )
            node%clients(j+1) = node%clients(j)
            node%links(j+1)%ptr => node%links(j)%ptr
            j = j - 1
        end do
        node%clients(j+1) = pclient
        node%links(j+1)%ptr => child 
        node%num = node%num + 1
    end subroutine insert_node
    subroutine split_node(new_client, pclient, pos, node, child, new_node)
        type(client), intent(in) :: new_client
        type(client), intent(inout) :: pclient
        integer, intent(in) :: pos
        type(BtreeNode), pointer, intent(inout) :: node, new_node
        type(BtreeNode), pointer, intent(in) :: child
        integer :: median, i, j
        if ( pos > 2 ) then
            median = 3
        else
            median = 2
        end if
        if ( .NOT. associated(new_node) ) then
            allocate(new_node)
            do i = 0, 4
                new_node%links(i)%ptr => null()
            end do
        end if
        j = median + 1
        do while ( j <= 4 )
            new_node%clients(j-median) = node%clients(j)
            new_node%links(j-median)%ptr => node%links(j)%ptr
            j = j + 1
        end do
        node%num = median
        new_node%num = 4 - median
        if ( pos <= 2 ) then
            call insert_node(new_client, pos, node, child)
        else
            call insert_node(new_client, pos - median, new_node, child)
        end if
        pclient = node%clients(node%num)
        new_node%links(0)%ptr => node%links(node%num)%ptr
        node%num = node%num - 1
    end subroutine split_node
    subroutine traversal(this, node)
        class(Btree_clients), intent(inout) :: this
        type(BtreeNode), pointer, intent(in) :: node
        integer :: i
        if ( associated(node) ) then
            write(*,'(A)', advance='no') "["
            i = 0
            write(*,'(I2, A)', advance='no') node%id, " "
            do while (i < node%num)
                write(*,'(A, A)', advance='no') node%clients(i+1)%name, ' '
                i = i + 1
            end do
            do i = 0, node%num
                call this%traversal(node%links(i)%ptr)
            end do
            write(*,'(A)', advance='no') "]"
        end if    
    end subroutine traversal
    subroutine clients_dot(this, node,  unit)
        class(Btree_clients), intent(inout) :: this
        type(BtreeNode), pointer, intent(in) :: node
        integer, intent(in) :: unit
        integer :: i
        if ( associated(node) ) then
            i = 0
            write(unit, '(A, I0, A)', advance='no') 'node_', node%id, ' [label=<<TABLE><TR>'
            do while(i < node%num)
                write(unit,'(A, A, A)', advance='no') '<TD>', node%clients(i+1)%name, '</TD>'
                i = i + 1
            end do
            write(unit, '(A)') '</TR></TABLE>>, shape="record"];'
            do i = 0, node%num
                call this%clients_dot(node%links(i)%ptr, unit)
                if ( associated(node%links(i)%ptr) ) then
                    write(unit, '(A, I0)', advance='no') 'node_', node%id
                    write(unit, '(A, I0)', advance='no') ' -> node_', node%links(i)%ptr%id
                    write(unit, '(A)') ' ;'
                end if
            end do
        end if   
    end subroutine clients_dot
end module clients