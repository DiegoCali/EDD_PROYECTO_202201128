module block_chain
    use routes, only: result_list, result
    use merkle_tree
    use branch_avl
    implicit none
    integer :: block_id = 0
    type block 
        integer :: index
        integer :: nonce
        character(:), allocatable :: timestamp
        type(result_list), pointer :: data
        character(len=256) :: previous_hash
        character(len=256) :: root_merkle
        character(len=256) :: hash
        type(block), pointer :: next => null()
        type(b_avl), pointer :: branches => null()
    contains
        procedure :: generate_block
    end type block
    type chainer
        type(block), pointer :: head => null()
        type(block), pointer :: tail => null()
    end type chainer
contains
    subroutine generate_block(this, new_data, new_branches)
    class(block), intent(inout) :: this
    type(result_list), pointer :: new_data
    type(b_avl), pointer :: new_branches
    type(result), pointer :: current
    type(branch), pointer :: b_origin, b_destination
    type(merkle) :: new_merkle
    integer, dimension(8) :: values 
    character(20) :: timestamp
    call date_and_time(values=values)
    write(timestamp, '(I0, A, I0, A, I0, A, I0, A, I0, A, I0)') values(3), '-', values(2) &
        , '-', values(1), '::', values(5), ':', values(6), ':', values(7)
    this%nonce = 4560
    this%timestamp = trim(timestamp)
    this%data => new_data
    this%branches => new_branches
    current => new_data%head
    do while ( associated(current) )
        b_origin => new_branches%search_branch(current%id)
        if ( associated(b_origin) ) then
            if ( associated(current%next) ) then
                b_destination => new_branches%search_branch(current%next%id)
                if ( associated(b_destination) ) then
                    call new_merkle%add_data(b_origin%key, b_origin%place, b_destination%key,&
                    b_destination%place, current%next%weight*80)
                end if
            end if
        end if
        current => current%next
    end do
    call new_merkle%generate()
    call new_merkle%merkle_dot()
    this%root_merkle = new_merkle%get_head_hash()
    end subroutine generate_block
end module block_chain