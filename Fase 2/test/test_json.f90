program jtest
    use json_module
    use json_kinds, only: ck
    use pixels
    use layers
    use images
    use albums
    implicit none
    type(json_file)                        :: json 
    type(json_value), pointer              :: list_pointer, img, img_att, mat_p, pixel_p, pixel_att
    type(json_core)                        :: core
    character(len=100)                     :: filename
    integer                                :: i, j, size, id_layer, matrix_size, x, y, vector_size
    logical                                :: found 
    character(kind=CK, len=:), allocatable :: color
    type(layer), pointer                   :: p_layer
    type(pixel_matrix), pointer            :: p_matrix
    type(layers_tree)                      :: img_layers_tree
    type(layers_tree), pointer             :: searched_image_layers
    type(image), pointer                   :: img_p, searched_image
    type(image_avl)                        :: imgs_avl
    type(album_list)                       :: albums_list    
    !read(*, '(A)') filename
    filename = 'files/layers.json'
    call json%initialize()
    call json%load(filename=trim(filename))

    call json%info('', n_children=size)

    call json%get_core(core)
    call json%get('', list_pointer, found=found)

    do i = 1, size
        call core%get_child(list_pointer, i, img, found=found) 
        call core%get_child(img, 'id_capa', img_att, found=found)
        if (found) then
            call core%get(img_att, id_layer)
        end if
        call core%get_child(img, 'pixeles', img_att, found=found)
        if (found) then
            call core%info(img_att, n_children=matrix_size)
            mat_p => img_att
            if (found) then
                allocate(p_matrix)
                do j = 1, matrix_size
                    call core%get_child(mat_p, j, pixel_p, found=found)
                    call core%get_child(pixel_p, 'columna', pixel_att, found=found)
                    if (found) then
                        call core%get(pixel_att, x)
                    end if
                    call core%get_child(pixel_p, 'fila', pixel_att, found=found)
                    if (found) then
                        call core%get(pixel_att, y)
                    end if
                    call core%get_child(pixel_p, 'color', pixel_att, found=found)
                    if (found) then
                        call core%get(pixel_att, color)
                    end if
                    call p_matrix%insert(x, y, .TRUE., color)
                end do
            end if
        end if
        allocate(p_layer)
        p_layer = layer(id_layer, matrix_size, p_matrix)
        call img_layers_tree%add(p_layer)
    end do
    call json%destroy()
    filename = 'files/img.json'
    call json%initialize()
    call json%load(filename=trim(filename))
    call json%info('', n_children=size)
    call json%get_core(core)
    call json%get('', list_pointer, found=found)
    do i = 1, size
        call core%get_child(list_pointer, i, img, found=found)
        call core%get_child(img, 'id', img_att, found=found)
        if (found) then
            allocate(img_p)
            call core%get(img_att, img_p%id)
        end if
        call core%get_child(img, 'capas', img_att, found=found)
        if ( found ) then
            call core%info(img_att, n_children=vector_size)
            do j = 1, vector_size
                call core%get_child(img_att, j, img, found=found)
                call core%get(img, id_layer)
                call img_p%add_layer(img_layers_tree%search(id_layer))
            end do
        end if
        call imgs_avl%add_img(img_p)
    end do
    call json%destroy()
    open(1, file='outputs/imgs_avl_subtree.dot', status='replace')
    call imgs_avl%gen_tree_subtree(6, 1)
    close(1)
    call execute_command_line('dot -Tsvg outputs/imgs_avl_subtree.dot -o outputs/imgs_avl_subtree.svg')
    ! open(1, file='outputs/imgs_avl.dot', status='replace')
    ! call imgs_avl%get_dot(imgs_avl%root, 1)
    !  close(1)
    !  call execute_command_line('dot -Tsvg outputs/imgs_avl.dot -o outputs/imgs_avl.svg')
    ! print *, '----------After deletion----------'
    ! call imgs_avl%delete_img(3)
    ! call imgs_avl%delete_img(3)
    ! open(1, file='outputs/imgs_avl_after.dot', status='replace')
    ! call imgs_avl%get_dot(imgs_avl%root, 1)
    ! close(1)
    ! call execute_command_line('dot -Tsvg outputs/imgs_avl_after.dot -o outputs/imgs_avl_after.svg')
    !call albums_list%new_album('Album 1')
    !searched_image => imgs_avl%search_img(imgs_avl%root, 3)
    !call albums_list%add_image_to_album(0, searched_image)
    !searched_image => imgs_avl%search_img(imgs_avl%root, 6)
    !call albums_list%add_image_to_album(0, searched_image)
    !call albums_list%new_album('Album 2')
    !searched_image => imgs_avl%search_img(imgs_avl%root, 1)
    !call albums_list%add_image_to_album(1, searched_image)
    !searched_image => imgs_avl%search_img(imgs_avl%root, 2)
    !call albums_list%add_image_to_album(1, searched_image)
    !call albums_list%show_albums()
    !call albums_list%show_album_images(0)
    !call albums_list%show_album_images(1)
    !print *, '----------Removing image 3 from album 1----------'
    !call albums_list%remove_image_from_album(0, 3)
    !call albums_list%show_albums()
    !call albums_list%show_album_images(0)
    !call albums_list%show_album_images(1)
    !print *, '----------Removing album 0 from album list-------'
    !call albums_list%remove_album(0)
    !call albums_list%show_albums()
    !call albums_list%show_album_images(0)
    !call albums_list%show_album_images(1)
end program jtest