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
    character(:), allocatable :: nombre 

    integer                   :: i, j, p_imgs, g_imgs, size    
    logical                   :: found

    allocate(client_queue)      

    call json%initialize()    
    call json%load(filename='test.json') 

    call json%info('',n_children=size)

    call json%get_core(jsonc)               
    call json%get('', listPointer, found)

    do i = 1, size                          
        call jsonc%get_child(listPointer, i, personPointer, found = found) 
        allocate(images_stack)                               

        call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)

        if (found) then                     
            call jsonc%get(attributePointer, g_imgs)
            do j = 1, g_imgs
                call images_stack%push("G")
            end do
        end if

        call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)

        if (found) then                     
            call jsonc%get(attributePointer, p_imgs)
            do j = 1, p_imgs
                call images_stack%push("p")
            end do
        end if

        call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found) 

        if (found) then                     
            call jsonc%get(attributePointer, nombre)                  
            call client_queue%enqueue(nombre, images_stack)
        end if

    end do

    call client_queue%self_print()
    call json%destroy() 
end program json_childs