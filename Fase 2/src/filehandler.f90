module filehandler
    use json_module
    use json_kinds, only: ck
    use images
    use clients
    use layers
    use pixels
    use albums
    implicit none
    type :: fhandler
        type(album_list), pointer :: albums_db => null()
        type(image_avl), pointer :: images_db => null()
        type(Btree_clients), pointer :: clients_db => null()
        type(layers_tree), pointer :: layers_db => null()
    contains
        procedure :: initialize_admin
        procedure :: initialize_user
        procedure :: set_user
        procedure :: read_imgs
        procedure :: read_layers
        procedure :: read_albums
        procedure :: read_clients
    end type fhandler
contains
    subroutine initialize_admin(this)
        class(fhandler), intent(inout) :: this
        call this%read_clients()
    end subroutine initialize_admin
    subroutine initialize_user(this)
        class(fhandler), intent(inout) :: this
        call this%read_layers()
        call this%read_imgs()
        call this%read_albums()
        print *, 'OK'
    end subroutine initialize_user
    subroutine set_user(this, user)
        class(fhandler), intent(inout) :: this
        type(client), pointer, intent(in) :: user
        this%albums_db => user%list_albums
        this%images_db => user%all_images
        this%layers_db => user%all_layers
    end subroutine set_user
    subroutine read_imgs(this)
        class(fhandler), intent(inout) :: this
        type(json_file) :: json
        type(json_core) :: jsonc 
        type(image), pointer :: new_img
        type(layer), pointer :: searched_layer
        type(json_value), pointer :: list_p, img_p, attr_p, layer_p
        integer :: i, j, size, vector_size, id_layer
        logical :: found
        call json%initialize()
        call json%load(filename='files/img.json')
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)
        do i = 1, size
            call jsonc%get_child(list_p, i, img_p, found=found)
            call jsonc%get_child(img_p, 'id', attr_p, found=found)
            if ( found ) then
                allocate(new_img)
                call jsonc%get(attr_p, new_img%id)
            end if
            call jsonc%get_child(img_p, 'capas', attr_p, found=found)
            if ( found ) then
                call jsonc%info(attr_p, n_children=vector_size)
                do j = 1, vector_size
                    call jsonc%get_child(attr_p, j, layer_p, found=found)
                    call jsonc%get(layer_p, id_layer)
                    searched_layer => this%layers_db%search(id_layer)
                    if ( associated(searched_layer) ) then
                        call new_img%add_layer(searched_layer)
                    else 
                        print *, 'Layer not found'
                    end if
                end do
            end if
            call this%images_db%add_img(new_img)
        end do
        call json%destroy()
    end subroutine read_imgs
    subroutine read_layers(this)
        class(fhandler), intent(inout) :: this
        type(json_file) :: json
        type(json_core) :: jsonc
        type(layer), pointer :: new_layer
        type(pixel_matrix), pointer :: new_matrix
        type(json_value), pointer :: list_p, layer_p, attr_p, mat_p, pixel_p, pixel_att
        integer :: i, j, size, id_layer, m_size, x, y
        logical :: found
        character(kind=ck, len=:), allocatable :: color
        call json%initialize()
        call json%load(filename='files/layers.json')
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)
        do i = 1, size
            call jsonc%get_child(list_p, i, layer_p, found=found)
            call jsonc%get_child(layer_p, 'id_capa', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, id_layer)
            end if
            call jsonc%get_child(layer_p, 'pixeles', attr_p, found=found)
            if ( found ) then
                call jsonc%info(attr_p, n_children=m_size)
                mat_p => attr_p
                if ( found ) then
                    allocate(new_matrix)
                    do j = 1, m_size
                        call jsonc%get_child(mat_p, j, pixel_p, found=found)
                        call jsonc%get_child(pixel_p, 'columna', pixel_att, found=found)
                        if ( found ) then
                            call jsonc%get(pixel_att, x)
                        end if
                        call jsonc%get_child(pixel_p, 'fila', pixel_att, found=found)
                        if ( found ) then
                            call jsonc%get(pixel_att, y)
                        end if
                        call jsonc%get_child(pixel_p, 'color', pixel_att, found=found)
                        if ( found ) then
                            call jsonc%get(pixel_att, color)
                        end if
                        call new_matrix%insert(x, y, .TRUE., color)
                    end do
                end if
            end if
            allocate(new_layer)
            new_layer = layer(id_layer, m_size, new_matrix)
            call this%layers_db%add(new_layer)
        end do
        call json%destroy()        
    end subroutine read_layers
    subroutine read_clients(this)
        class(fhandler), intent(inout) :: this
        type(json_file) :: json
        type(json_core) :: jsonc
        type(client), pointer :: new_client
        type(json_value), pointer :: list_p, client_p, attr_p
        integer :: i, j, size
        character(:), allocatable :: dpi_str, name, password
        integer*8 :: dpi
        logical :: found
        call json%initialize()
        call json%load(filename='files/clients.json')
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)
        do i = 1, size
            call jsonc%get_child(list_p, i, client_p, found=found)
            call jsonc%get_child(client_p, 'dpi', attr_p, found=found)            
            if ( found ) then
                call jsonc%get(attr_p, dpi_str)
                read(dpi_str, '(I13)') dpi
            end if
            call jsonc%get_child(client_p, 'nombre_cliente', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, name)
            end if
            call jsonc%get_child(client_p, 'password', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, password)
            end if
            call this%clients_db%add_client(client(name, dpi, password))
        end do
        call json%destroy()        
    end subroutine read_clients
    subroutine read_albums(this)
        class(fhandler), intent(inout) :: this
        type(json_file) :: json
        type(json_core) :: jsonc
        type(image), pointer :: found_img
        type(json_value), pointer :: list_p, album_p, attr_p, img_p, img_att
        integer :: i, j, size, img_id, album_size
        character(:), allocatable :: name
        logical :: found
        call json%initialize()
        call json%load(filename='files/albums.json')
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)
        do i = 1, size
            call jsonc%get_child(list_p, i, album_p, found=found)
            call jsonc%get_child(album_p, 'nombre_album', attr_p, found=found)
            if ( found ) then
                call jsonc%get(attr_p, name)
            end if
            call this%albums_db%new_album(name)
            call jsonc%get_child(album_p, 'imgs', img_p, found=found)
            if (found) then
                call jsonc%info(img_p, n_children=album_size)
                do j = 1, album_size
                    call jsonc%get_child(img_p, j, img_att, found=found)
                    call jsonc%get(img_att, img_id)
                    found_img => this%images_db%search_img(this%images_db%root, img_id)
                    if ( associated(found_img) ) then
                        call this%albums_db%add_image_to_album(i-1, found_img)
                    else 
                        print *, 'Image not found'
                    end if
                end do
            end if
        end do
        call json%destroy()        
    end subroutine read_albums
end module filehandler