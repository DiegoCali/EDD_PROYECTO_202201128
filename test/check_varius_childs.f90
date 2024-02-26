program json_childs
    use json_module
    use client_queue
    use image_stack
    implicit none

    type(json_file)           :: json   
    type(json_value), pointer :: listPointer, personPointer, attributePointer  
    type(json_core)           :: jsonc  
    type(queue), pointer      :: client_queue
    type(stack), pointer      :: images_stack
    character(:), allocatable :: nombre, p_imgs_str, g_imgs_str
    character(len=100) 	      :: file_name

    integer                   :: i, j, p_imgs, g_imgs, size    
    logical                   :: found

    allocate(client_queue)      
    
    read (*, "(A)") file_name
	
    print *, file_name
    call json%initialize()    
    call json%load(filename=trim(file_name)) 

    call json%info('',n_children=size)

    call json%get_core(jsonc)               
    call json%get('', listPointer, found)

    do i = 1, size                          
        call jsonc%get_child(listPointer, i, personPointer, found = found) 
        allocate(images_stack)                               

        call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)

        if (found) then                     
            call jsonc%get(attributePointer, g_imgs_str)
            read (g_imgs_str, *) g_imgs
            do j = 1, g_imgs
                call images_stack%push(2)
            end do
        end if

        call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)

        if (found) then                     
            call jsonc%get(attributePointer, p_imgs_str)
            read (p_imgs_str, *) p_imgs
            do j = 1, p_imgs
                call images_stack%push(1)
            end do
        end if

        call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found) 

        if (found) then                     
            call jsonc%get(attributePointer, nombre)                  
            call client_queue%enqueue(nombre, images_stack, 0, g_imgs, p_imgs)
        end if

    end do
    call json%destroy() 
    call client_queue%self_print()
end program json_childs
