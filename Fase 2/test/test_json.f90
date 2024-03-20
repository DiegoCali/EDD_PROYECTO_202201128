program jtest
    use json_module
    use json_kinds, only: ck
    use pixels
    use layers
    use images
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
    type(image), pointer                   :: img_p
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
                call img_p%layers%add_copied_val(img_layers_tree%search(id_layer))
            end do
        end if
        print *, 'Finished image: ', img_p%id
        call img_p%layers%inorder(img_p%layers%root)
        print *, ''
    end do
end program jtest