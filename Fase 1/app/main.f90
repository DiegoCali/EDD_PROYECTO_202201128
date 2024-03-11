program main
  use EDD_PROYECT_202201128, only: say_hello
  use randomizer
  use json_module
  use client_queue
  use image_stack
  use printer_queue
  use window_list
  use waiting_list
  use attended_clients
  implicit none
  type(json_file)           :: json   
  type(json_value), pointer :: listPointer, personPointer, attributePointer  
  type(json_core)           :: jsonc  
  type(queue) 		    :: client_queue
  type(stack), pointer      :: images_stack
  type(windows)             :: windows_list
  character(:), allocatable :: nombre, p_imgs_str, g_imgs_str
  character(len=100)	    :: file_name
  integer                   :: i, j, p_imgs, g_imgs, size, windows_amount, step, amount_clients, option, unit
  logical        	    :: running, found
  type(rand)      	    :: randit
  type(attended_list)       :: clients_att
  type(wait_list), target   :: clients_waiting
  type(printer), target     :: g_printer
  type(printer), target     :: p_printer
  type(window), pointer     :: temp_window
  type(client), pointer     :: temp_client
  
  ! Step initialization
  step = 0
  unit = 69
  ! Randit initialization
  randit%names = (/"Diego", "Carly", "Anaya", "Clint", "Ethan"/)
  randit%last_names = (/"Smith", "Brown", "Clark", "White", "Moore"/)
  ! Lists initializations
  call client_queue%start(0)
  windows_list%waiting_queue => clients_waiting ! linking the window list and waiting list  *
  windows_list%big_printer => g_printer         ! linking the window list and big printer   *
  windows_list%small_printer => p_printer       ! linking the window list and small printer * 
  g_printer%waiting_clients => clients_waiting  ! linking big printer and waiting clients   *
  p_printer%waiting_clients => clients_waiting  ! linking small printer and waiting clients *
  
  running = .TRUE.
  call say_hello()
  do while(running)
    print *, "POR FAVOR SELECCIONA UNA OPCION:"
    print *, "1. Parametros iniciales"
    print *, "2. Ejecutar paso"
    print *, "3. Estado de las estructuras de memoria"
    print *, "4. Reportes"
    print *, "5. Sobre mi"
    print *, "6. Salir"
    read *, option
    select case (option)
      case (1)
        print *, "1. Carga masiva de clientes"
        print *, "2. Cantidad de ventanillas"
        read *, option
        select case (option)
          case(1)    
            print *, "Ingrese el nombre del archivo:"
            read (*, "(A)") file_name
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
           call client_queue%self_print()
    	   call json%destroy() 
          case (2)
            print *, "Ingrese la cantidad de ventanillas:"
            read *, windows_amount
            do i = 1, windows_amount
            	call windows_list%create(i)
            end do
            call windows_list%self_print()
          case default
            print *, "Seleccione una opcion que este disponible."
       end select
      case (2)
      	amount_clients = randit%get_rand_number(0, 2)
      	do i=0, amount_clients
      		allocate(images_stack)
      		g_imgs = randit%get_rand_number(0, 2)
      		do j=0, g_imgs
      			call images_stack%push(2)
      		end do
      		p_imgs = randit%get_rand_number(0, 2)
      		do j=0, p_imgs
      			call images_stack%push(1)
      		end do
      		nombre = randit%get_rand_name()
      		call client_queue%enqueue(nombre, images_stack, step, g_imgs, p_imgs)
      	end do
        write(*, '(a20,i2,a15)') "//-------------PASO:", step, "-------------\\"
        call g_printer%execute_step()
        call p_printer%execute_step()
        call windows_list%get_images()
        if (associated(client_queue%head)) then
            if (windows_list%opened_windows()) then
                temp_window => windows_list%search_free_window()    
                temp_window%client => client_queue%check()
            end if
        end if
        if (associated(clients_waiting%head)) then
            temp_client => clients_waiting%check()
            if (associated(temp_client)) then
            	temp_client%steps = step - temp_client%steps
        	call clients_att%push_node(temp_client)
            end if
        end if
        print *, "---------COLA DE CLIENTES---------"
        call client_queue%self_print()
        print *, "-----------ATENCIONES-------------"
        call windows_list%print_attending()
        print *, "----------COLA DE ESPERA----------"
        call clients_waiting%print()
        print *, "-----------VENTANILLAS------------"
        call windows_list%self_print()
        print *, "---------IMPRESORA GRANDE---------"
        call g_printer%show_self()
        print *, "--------IMPRESORA PEQUEÑA---------"
        call p_printer%show_self()
        print *, "----------------------------------"
      case (3)
        open(unit,file='graph.dot', status='replace')
        write(unit, *) "digraph G{"
        call client_queue%graph(unit)
        call windows_list%graph_self(unit)
        call clients_waiting%self_graph(unit)
        call p_printer%make_graph(unit, 'p')
        call g_printer%make_graph(unit, 'G')
        call clients_att%graph_attended(unit)
        write(unit, *) "}"
        close(unit)
        call execute_command_line('dot -Tsvg graph.dot > output.svg')
        call execute_command_line('eog output.svg')
      case (4)
        print *, "1. Top 5 clientes con mayor cantidad de imagenes grandes"
        print *, "2. Top 5 clientes con mayor cantidad de imagenes pequeñas"
        print *, "3. Informacion del cliente con mas pasos"
        print *, "4. Buscar informacion de un cliente (atendido)"
        read *, option
        select case(option)
        	case (1)
        		call clients_att%sort(.TRUE.)
        	case (2)
        		call clients_att%sort(.FALSE.)
        	case (3)
        		call clients_att%search_most_step()
        	case (4)
        		call clients_att%consult()
        	case default
        		print *, "Seleccione una opcion que este disponible."
        end select
      case (5)
        print *, "----------------------------------------"
        print *, "  Diego Felipe Cali Morales: 202201128"
        print *, "----------------------------------------"
      case (6)
        running = .FALSE.
      case default
      	print *, "Seleccione una opcion que este disponible."
    end select
  step = step + 1
  end do
  print *, "//----------Programa Terminado----------\\"
end program main
