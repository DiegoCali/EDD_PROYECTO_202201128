module tech_hash
    implicit none
    type tech
        integer*8 :: dpi ! key
        character(:), allocatable :: name, last_name, address, gender, phone ! Data
        integer :: works_done = 0
    end type tech
    type :: hash
        integer :: n ! Number of elements
        integer :: m ! Table size
        integer :: mini, maxi ! Min and max precentages
        type(tech), dimension(:), allocatable :: h ! Hash table
    contains 
        procedure :: init 
        procedure :: dispersion
        procedure :: collision
        procedure :: insert
        procedure :: rehashing 
        procedure :: search
        procedure :: show 
        procedure :: hash_dot
    end type hash
contains
    subroutine init(this,  m, mini, maxi)
        class(hash), intent(inout) :: this
        integer, intent(in) :: m, mini, maxi
        this%m = m
        this%mini = mini
        this%maxi = maxi
        this%n = 0
        if ( allocated(this%h) ) then
            deallocate(this%h)
        end if
        allocate(this%h(m))
        this%h = tech(-1, 'null', 'null', 'null', 'null', 'null')
    end subroutine init
    function dispersion(this,  k) result(disp)
        class(hash), intent(inout) :: this
        integer*8, intent(in) :: k
        integer :: disp
        disp = mod(k, this%m)
    end function dispersion
    function collision(this,  k, i) result(coll)
        class(hash), intent(inout) :: this
        integer*8, intent(in) :: k
        integer, intent(in) :: i
        integer :: coll
        coll = mod((mod(k, 7) + 1)*i, this%m)
    end function collision
    subroutine insert(this, t)
        class(hash), intent(inout) :: this
        type(tech), intent(in) :: t
        integer :: i
        integer*8 :: k, d
        k = t%dpi
        i = 1
        d = dispersion(this, k)
        if ( d == 0 ) then
            d = 1
        end if
        do while (this%h(d)%dpi /= -1)
            d = collision(this, k, i)
            i = i + 1
        end do    
        this%h(d) = t
        this%n = this%n + 1
        call this%rehashing()            
    end subroutine insert
    subroutine rehashing(this)
        class(hash), intent(inout) :: this
        integer :: i, mprev
        type(tech), dimension(:), allocatable :: temp 
        if ( this%n * 100 / this%m >= this%maxi ) then
            allocate(temp(this%m))
            temp = this%h
            mprev = this%m            
            this%m = this%m * 2            
            call this%init(this%m, this%mini, this%maxi)
            do i = 1, mprev
                if ( temp(i)%dpi /= -1 ) then
                    call this%insert(temp(i))
                end if
            end do
        end if               
    end subroutine rehashing
    subroutine search(this, k)
        class(hash), intent(inout) :: this
        integer*8, intent(in) :: k
        integer :: i, d
        i = 0
        d = dispersion(this, k)
        do while (this%h(d)%dpi /= k)
            d = collision(this, k, i)
            i = i + 1
        end do
        print *, this%h(d)%name, this%h(d)%last_name, this%h(d)%address, this%h(d)%gender, this%h(d)%phone            
    end subroutine search
    subroutine show(this)
        class(hash), intent(inout) :: this
        integer :: i        
        do i = 1, this%m
            if ( this%h(i)%dpi /= -1 ) then
                write(*, '(I13, A, A)') this%h(i)%dpi, "  " ,this%h(i)%name
            end if
        end do              
    end subroutine show
    subroutine hash_dot(this)
        class(hash), intent(inout) :: this        
        integer :: i, unit 
        unit = 10
        open(unit, file='outputs/hash_table.dot', status='replace')

        write(unit, '(A)') 'digraph Hash_Table {'

        write(unit, '(A)') 'rankdir=LR;'
        write(unit, '(A)') 'node [shape=record];'

        write(unit, '(A)') 'node0 [ ' 

        write(unit, '(A)', advance='no') ' label="'

        do i = 1, this%m
            if ( i == this%m ) then
                write(unit, '(A, I0, A, I0)', advance='no') '<f', i-1,'>', i
            else 
                write(unit, '(A, I0, A, I0, A)', advance='no') '<f', i-1,'>', i, '|'
            end if
        end do

        write(unit, '(A)') '"];'

        do i = 1, this%m
            if ( this%h(i)%dpi /= -1 ) then                
                write(unit, '(A, I0, A, A, A)') 'node0:f',i-1,' -> "', this%h(i)%name, '";'                   
            end if        
        end do

        write(unit, '(A)') '}'  
        
        close(unit)

        call execute_command_line('dot -Tsvg outputs/hash_table.dot -o outputs/hash_table.svg')
    end subroutine hash_dot
end module tech_hash