module tech_hash
    implicit none
    type tech
        integer*8 :: dpi 
        character(:), allocatable :: name, last_name, address, gender, phone
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
            print *, 'New size: ', this%m
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
        write(*, '(A)', advance='no') '['
        do i = 1, this%m
            write(*, '(A, A)', advance='no') this%h(i)%name, " "
        end do      
        write(*, '(A)', advance='no') ']'
    end subroutine show
end module tech_hash