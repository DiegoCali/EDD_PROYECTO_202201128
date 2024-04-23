module file_handler
    use json_module
    use json_kinds, only: ck
    implicit none
    type, public :: fhandler        
    contains
        procedure :: read_techs    
    end type fhandler
contains
    subroutine read_techs(file_route)     
        character(len=*), intent(in) :: file_route
        type(fhandler) :: self
        type(json_file) :: json
        type(json_core) :: json_core
        type(json_value), pointer :: list_p, techs_p, attr_p
        character(kind=ck, len=13) :: dpi 
        integer(len=8) :: dpi_value
        character(kind=ck, len=:) :: name, last_name, address, gender, cell
        integer :: i        
    end subroutine read_techs
end module file_handler