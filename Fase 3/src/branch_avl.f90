module branch_avl
    use tech_hash
    implicit none
    type branch
        integer :: key
        character(:), allocatable :: place, address, password
        type(branch), pointer :: left => null()
        type(branch), pointer :: right => null()
        integer :: height
        integer :: total_costs = 0
        integer :: total_revenue = 0
        type(hash), pointer :: hash_table => null()
    end type branch
    type b_avl 
        type(branch), pointer :: root => null()
        integer :: total = 0
        integer :: total_costs = 0
        integer :: total_revenue = 0
    contains
        procedure :: add_branch 
        procedure :: add_branch_rec
        procedure :: srl
        procedure :: srr
        procedure :: drl 
        procedure :: drr 
        procedure :: get_height
        procedure :: get_max
        procedure :: search_branch
        procedure :: get_dot
        procedure :: get_dot_rec
        procedure :: print_self 
        procedure :: print_self_rec   
        procedure :: get_totals
        procedure :: get_totals_rec    
    end type b_avl
contains
    subroutine add_branch(this, new_branch)        
        class(b_avl), intent(inout) :: this
        type(branch), pointer, intent(in) :: new_branch

        if ( associated(this%root) ) then
            call this%add_branch_rec(new_branch, this%root)            
        else 
            this%root => new_branch
        end if   

    end subroutine add_branch    
    subroutine add_branch_rec(this, new_branch, tmp)
        class(b_avl), intent(inout) :: this 
        type(branch), pointer, intent(in) :: new_branch
        type(branch), pointer :: tmp 
        integer :: r, l, m

        if ( .NOT. associated(tmp) ) then 
            tmp => new_branch
        else if ( new_branch%key < tmp%key ) then 
            call this%add_branch_rec(new_branch, tmp%left)
            if ( (this%get_height(tmp%left) - this%get_height(tmp%right)) == 2 ) then
                if ( new_branch%key < tmp%left%key ) then
                    tmp => this%srl(tmp)
                else 
                    tmp => this%drl(tmp)
                end if                
            end if
        else 
            call this%add_branch_rec(new_branch, tmp%right)
            if ( (this%get_height(tmp%right) - this%get_height(tmp%left)) == 2 ) then
                if ( new_branch%key > tmp%right%key ) then
                    tmp => this%srr(tmp)
                else 
                    tmp => this%drr(tmp)
                end if
            end if
        end if   
        r = this%get_height(tmp%right)
        l = this%get_height(tmp%left)
        m = this%get_max(r, l)
        tmp%height = m + 1      
    end subroutine add_branch_rec
    function srl(this, t1) result(t2)        
        class(b_avl), intent(in) :: this 
        type(branch), pointer, intent(in) :: t1 
        type(branch), pointer :: t2
        t2 => t1%left
        t1%left => t2%right
        t2%right => t1 
        t1%height = this%get_max(this%get_height(t1%left), this%get_height(t1%right)) + 1
        t2%height = this%get_max(this%get_height(t2%left), t1%height) + 1
    end function srl
    function srr(this, t1) result(t2)
        class(b_avl), intent(in) :: this 
        type(branch), pointer, intent(in) :: t1 
        type(branch), pointer :: t2 
        t2 => t1%right
        t1%right => t2%left
        t2%left => t1 
        t1%height = this%get_max(this%get_height(t1%left), this%get_height(t1%right)) + 1
        t2%height = this%get_max(this%get_height(t2%right), t1%height) + 1
    end function srr
    function drl(this, tmp) result(res)
        class(b_avl), intent(in) :: this 
        type(branch), pointer, intent(in) :: tmp 
        type(branch), pointer :: res 
        tmp%left => this%srr(tmp%left)
        res => this%srl(tmp)        
    end function drl
    function drr(this, tmp) result(res)
        class(b_avl), intent(in) :: this 
        type(branch), pointer, intent(in) :: tmp 
        type(branch), pointer :: res 
        tmp%left => this%srl(tmp%left)
        res => this%srr(tmp)        
    end function drr
    function get_max(this, val1, val2) result(res)
        class(b_avl), intent(in) :: this 
        integer, intent(in) :: val1, val2 
        integer :: res
        res = merge(val1, val2, val1 > val2)   
    end function get_max
    function get_height(this, tmp) result(res)
        class(b_avl), intent(in) :: this 
        type(branch), pointer, intent(in) :: tmp
        integer :: res 
        if ( .NOT. associated(tmp) ) then
            res = -1
        else
            res = tmp%height
        end if   
    end function get_height
    function search_branch(this, id) result(tmp)
        class(b_avl), intent(in) :: this
        integer, intent(in) :: id
        type(branch), pointer :: tmp
        tmp => this%root
        do while ( associated(tmp) )
            if ( id == tmp%key ) then
                return
            else if ( id < tmp%key ) then
                tmp => tmp%left
            else
                tmp => tmp%right
            end if
        end do
        tmp => null()
    end function search_branch
    subroutine get_dot(this)
        class(b_avl), intent(in) :: this   
        open(78, file='outputs/branch_avl.dot', status='replace')             
        write(78, '(A)') 'digraph branch_avl {'
        call this%get_dot_rec(this%root, 78)
        write(78, '(A)') '}'
        close(78)
        print *, 'Closing file...'
        call execute_command_line('dot -Tsvg outputs/branch_avl.dot -o outputs/branch_avl.svg')
    end subroutine get_dot
    subroutine get_dot_rec(this, tmp, unit)
        class(b_avl), intent(in) :: this
        type(branch), pointer, intent(in) ::  tmp
        integer, intent(in) ::  unit
        if ( .NOT. associated(tmp) ) then
            return
        end if
        write(unit, '(A, I0, A, I0, A)') 'node_', tmp%key, '[label="branch_', tmp%key, '"];'        
        if ( associated(tmp%left) ) then
            write(unit, '(A, I0, A, I0, A)') 'node_', tmp%key, ' -> node_', tmp%left%key, ';'
        end if
        if ( associated(tmp%right) ) then
            write(unit, '(A, I0, A, I0, A)') 'node_', tmp%key, ' -> node_', tmp%right%key, ';'
        end if
        call this%get_dot_rec(tmp%left, unit)
        call this%get_dot_rec(tmp%right, unit)
    end subroutine get_dot_rec
    subroutine print_self(this)
        class(b_avl), intent(in) :: this
        if ( associated(this%root) ) then
            call this%print_self_rec(this%root)        
        end if
    end subroutine print_self
    recursive subroutine print_self_rec(this, tmp)
        class(b_avl), intent(in) :: this
        type(branch), pointer, intent(in) :: tmp
        if ( associated(tmp) ) then
            call this%print_self_rec(tmp%left)
            write(*, '(I0, A, A, A, A)') tmp%key, "  ", tmp%place, ",  ", tmp%address
            call this%print_self_rec(tmp%right)
        end if
    end subroutine print_self_rec
    subroutine get_totals(this)
        class(b_avl), intent(inout) :: this
        if ( associated(this%root) ) then
            call this%get_totals_rec(this%root)
        end if   
        print *, 'Total costs: ', this%total_costs
        print *, 'Total revenue: ', this%total_revenue
        print *, 'Total profit: ', this%total_revenue - this%total_costs 
    end subroutine get_totals
    recursive subroutine get_totals_rec(this, tmp)
        class(b_avl), intent(inout) :: this
        type(branch), pointer, intent(in) :: tmp
        if ( associated(tmp) ) then
            call this%get_totals_rec(tmp%left)
            this%total_costs = this%total_costs + tmp%total_costs
            this%total_revenue = this%total_revenue + tmp%total_revenue
            call this%get_totals_rec(tmp%right)
        end if    
    end subroutine get_totals_rec
end module branch_avl