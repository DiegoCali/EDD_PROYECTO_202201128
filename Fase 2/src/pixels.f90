module pixels    
    implicit none
    integer :: id = 0
    type :: pixel  
        integer :: id  
        integer :: x, y
        logical :: on = .FALSE.
        character(len=7) :: color
        type(pixel), pointer :: up => null()
        type(pixel), pointer :: down => null()        
        type(pixel), pointer :: right => null()
        type(pixel), pointer :: left => null()
    end type pixel
    type :: pixel_matrix
        type(pixel), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
    contains 
        procedure :: insert
        procedure :: self_print
        procedure :: search_row
        procedure :: search_column
        procedure :: node_exists
        procedure :: insert_column_header
        procedure :: insert_row_header
        procedure :: insert_in_row
        procedure :: insert_in_column
        procedure :: print_headers
        procedure :: get_value
        procedure :: get_node
        procedure :: graph_pixels
    end type pixel_matrix
contains
    subroutine insert(this, x, y, value, color)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: x, y
        logical, intent(in) :: value
        character(len=7), intent(in) :: color
        type(pixel), pointer :: new_pixel
        type(pixel), pointer :: row 
        type(pixel), pointer :: column    
        allocate(new_pixel)
        new_pixel = pixel(id, x, y, value, color)
        if (.NOT. associated(this%root)) then
            allocate(this%root)
            this%root = pixel(id, -1, -1, .FALSE., color)
        end if
        row => this%search_row(y)
        column => this%search_column(x)        
        if(y > this%height) this%height = y
        if(x > this%width) this%width = x        
        if (.NOT. this%node_exists(new_pixel)) then            
            if (.NOT. associated(column)) then                
                column => this%insert_column_header(x)
            end if            
            if (.NOT. associated(row)) then                 
                row => this%insert_row_header(y)
            end if            
            call this%insert_in_column(new_pixel, row)
            call this%insert_in_row(new_pixel, column)
        end if 
        id = id + 1      
    end subroutine insert    
    function search_row(this,  y) result(actual)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: y
        type(pixel), pointer :: actual
        actual => this%root
        do while (associated(actual))
            if (actual%y == y) return            
            actual => actual%down
        end do        
    end function search_row
    function search_column(this,  x) result(actual)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: x
        type(pixel), pointer :: actual
        actual => this%root
        do while (associated(actual))
            if (actual%x == x) return            
            actual => actual%right
        end do        
    end function search_column
    function node_exists(this, new_node) result(exists)
        class(pixel_matrix), intent(inout) :: this
        type(pixel), pointer, intent(in) :: new_node
        logical :: exists
        type(pixel), pointer :: row_header 
        type(pixel), pointer :: column
        row_header => this%root
        exists = .FALSE.        
        do while (associated(row_header))
            if (row_header%y == new_node%y) then
                column => row_header
                do while (associated(column))
                    if (column%x == new_node%x) then
                        exists = .TRUE.
                        return
                    end if
                    column => column%right                    
                end do
            end if  
            row_header => row_header%down                     
        end do        
    end function node_exists
    function insert_row_header(this, y) result(new_row_header)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: y
        type(pixel), pointer :: new_row_header
        character(len=7) :: color
        color = "#000000"
        allocate(new_row_header)
        new_row_header = pixel(id, -1, y, .FALSE., color)        
        call this%insert_in_row(new_row_header, this%root)
    end function insert_row_header
    subroutine insert_in_row(this,  new_node, row_header)
        class(pixel_matrix), intent(inout) :: this
        type(pixel), pointer, intent(in) :: new_node
        type(pixel), pointer, intent(in) :: row_header
        type(pixel), pointer :: current
        current => row_header
        do while (associated(current%down))
            if (new_node%y < current%down%y .AND. new_node%y > current%y) then
                new_node%down => current%down
                new_node%up => current
                current%down%up => new_node
                current%down => new_node 
                return               
            end if
            current => current%down
        end do
        if (.NOT. associated(current%down)) then
            current%down => new_node
            new_node%up => current
        end if
    end subroutine insert_in_row
    function insert_column_header(this, x) result(new_column_header)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: x
        type(pixel), pointer :: new_column_header
        character(len=7) :: color
        color = "#000000"
        allocate(new_column_header)
        new_column_header = pixel(id, x, -1, .FALSE., color)    
        call this%insert_in_column(new_column_header, this%root)
    end function insert_column_header   
    subroutine insert_in_column(this,  new_node, column_header)
        class(pixel_matrix), intent(inout) :: this
        type(pixel), pointer, intent(in) :: new_node
        type(pixel), pointer, intent(in) :: column_header
        type(pixel), pointer :: current
        current => column_header
        do while (associated(current%right))
            if (new_node%x < current%right%x .AND. new_node%x > current%x) then
                new_node%right => current%right
                new_node%left => current
                current%right%left => new_node
                current%right => new_node    
                return            
            end if
            current => current%right
        end do
        if (.NOT. associated(current%right)) then
            current%right => new_node
            new_node%left => current
        end if
    end subroutine insert_in_column 
    subroutine print_headers(this)
        class(pixel_matrix), intent(inout) :: this
        integer :: x 
        do x = -1, this%width
            write(*, '(I3)', advance='no') x
        end do    
    end subroutine print_headers
    function get_value(this, x, y) result(value)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: x, y
        type(pixel), pointer :: row
        type(pixel), pointer :: column
        logical :: value 
        value = .FALSE.
        row => this%root
        do while (associated(row))
            if (row%y == y) then
                column => row
                do while (associated(column))
                    if (column%x == x) then
                        value = column%on
                        return
                    end if
                    column => column%right
                end do
            end if
            row => row%down
        end do
    end function get_value
    function get_node(this, x, y) result(node)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: x, y
        type(pixel), pointer :: row
        type(pixel), pointer :: column
        type(pixel), pointer :: node        
        row => this%root
        do while (associated(row))
            if (row%y == y) then
                column => row
                do while (associated(column))
                    if (column%x == x) then
                        node => column
                        return
                    end if
                    column => column%right
                end do
            end if
            row => row%down
        end do        
    end function get_node
    subroutine self_print(this)
        class(pixel_matrix), intent(inout) :: this
        integer :: x, y
        logical :: value        
        call this%print_headers()
        do y = 0, this%height
            print *, ""
            write(*, '(I3)', advance='no') y            
            do x = 0, this%width
                value = this%get_value(x, y)
                if (value) then
                    write(*, '(A3)', advance='no') "X"
                else
                    write(*, '(A3)', advance='no') " "
                end if
            end do
        end do   
        print *, ""     
    end subroutine self_print
    subroutine graph_pixels(this, unit)
        class(pixel_matrix), intent(inout) :: this
        integer, intent(in) :: unit        
        integer :: i
        type(pixel), pointer :: actual, temp
        write(unit, '(A)') 'Node [shape=box];'
        write(unit, '(A)') 'root [label="ROOT", style=filled];'
        do i = 0, this%height
            write(unit, '(A, I0, A)') 'row_', i, '[label="ROW", style=filled];'
            if (i /= this%height) then
                write(unit, '(A, I0, A, I0)') 'row_', i, ' -> row_', i+1
            end if
        end do
        do i = 0, this%width
            write(unit, '(A, I0, A)') 'col_', i, '[label="COLUMN", style=filled];'
            if (i /= this%width) then
                write(unit, '(A, I0, A, I0)') 'col_', i, ' -> col_', i+1
            end if
        end do
        write(unit, '(A)', advance='no') '{ rank=same; root'
        do i = 0, this%width
            write(unit, '(A, I0)', advance='no') '; col_', i
        end do
        write(unit, '(A)') '}'
        actual => this%root        
        do while (associated(actual))
            temp => actual
            do while (associated(actual))
                if (actual%x == -1 .AND. actual%y == -1) then
                    write(unit, '(A)') 'root -> row_0'
                    write(unit, '(A)') 'root -> col_0'
                else if (actual%x == -1) then
                    if (associated(actual%right)) then
                        write(unit, '(A, I0, A, I0, A)') 'row_', actual%y, ' -> node_', actual%right%id, ''
                    end if
                else if (actual%y == -1) then
                    if (associated(actual%down)) then
                        write(unit, '(A, I0, A, I0)') 'col_', actual%x, ' -> node_', actual%down%id
                    end if
                else 
                    write(unit, '(A, I0, A, A, A)') 'node_', actual%id, '[label="X", style=filled, color="', actual%color, '"];'
                    if (associated(actual%up)) then
                        if (actual%up%y == -1) then
                            write(unit, '(A, I0, A, I0, A)') 'node_', actual%id, ' -> col_', actual%up%x, ''
                        else
                            write(unit, '(A, I0, A, I0, A)') 'node_', actual%id, ' -> node_', actual%up%id, ''
                        end if                        
                    end if
                    if (associated(actual%down)) then
                        write(unit, '(A, I0, A, I0, A)') 'node_', actual%id, ' -> node_', actual%down%id, ''
                    end if
                    if (associated(actual%right)) then
                        write(unit, '(A, I0, A, I0, A)') 'node_', actual%id, ' -> node_', actual%right%id, ''
                    end if
                    if (associated(actual%left)) then
                        if (actual%left%x == -1) then
                            write(unit, '(A, I0, A, I0, A)') 'node_', actual%id, ' -> row_', actual%left%y, ''
                        else
                            write(unit, '(A, I0, A, I0, A)') 'node_', actual%id, ' -> node_', actual%left%id, ''
                        end if
                    end if
                end if                
                actual => actual%right
            end do
            actual => temp
            if (actual%y /= -1) then
                do while(associated(actual))
                    if (actual%x == -1) then
                        write(unit, '(A, I0)', advance='no') '{ rank=same; row_', actual%y
                    else 
                        write(unit, '(A, I0)', advance='no') '; node_', actual%id
                    end if
                    actual => actual%right
                end do
                write(unit, '(A)') '}'
            end if         
            actual => temp%down
        end do
    end subroutine graph_pixels
end module pixels