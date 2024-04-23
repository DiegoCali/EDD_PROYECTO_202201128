module file_handler
    use json_module
    use json_kinds, only: ck
    use tech_hash
    implicit none
    type, public :: fhandler   
        type(hash), pointer :: techs_table => null() ! techs hash table     
    contains
        procedure :: connect 
        procedure :: read_techs    
    end type fhandler
contains
    subroutine connect(self,  tech_hash_table)
        class(fhandler), intent(inout) :: self
        type(hash), pointer :: tech_hash_table
        self%techs_table => tech_hash_table
        print *, 'Connection successful!'        
    end subroutine connect
    subroutine read_techs(self, file_route)   
        class(fhandler), intent(inout) :: self  
        character(len=*), intent(in) :: file_route        
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: list_p, techs_p, attr_p
        character(kind=ck, len=:), allocatable :: dpi 
        integer*8 :: dpi_value
        character(kind=ck, len=:), allocatable :: name, last_name, address, gender, cell
        integer :: i, size
        logical :: found

        call json%initialize()
        call json%load(filename=trim(file_route))
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found=found)

        do i = 1, size
            call jsonc%get_child(list_p, i, techs_p, found=found)
            call jsonc%get_child(techs_p, 'dpi', attr_p, found=found)
            
            if ( found ) then
                call jsonc%get(attr_p, dpi)
                read(dpi, '(I13)') dpi_value
            end if

            call jsonc%get_child(techs_p, 'nombre', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, name)
            end if

            call jsonc%get_child(techs_p, 'apellido', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, last_name)
            end if

            call jsonc%get_child(techs_p, 'genero', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, gender)
            end if

            call jsonc%get_child(techs_p, 'direccion', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, address)
            end if

            call jsonc%get_child(techs_p, 'telefono', attr_p, found=found)

            if ( found ) then
                call jsonc%get(attr_p, cell)
            end if

            call self%techs_table%insert(tech(dpi_value, name, last_name, address, gender, cell))
        end do
        call json%destroy()
    end subroutine read_techs
end module file_handler