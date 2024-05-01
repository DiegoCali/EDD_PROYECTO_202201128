module block_chain
    use routes, only: result_list, result
    use merkle_tree
    use branch_avl
    use sha256_module
    use json_module
    implicit none
    integer :: block_id = 0
    type block 
        integer :: index
        integer :: nonce
        character(:), allocatable :: timestamp
        type(result_list), pointer :: data
        character(len=256) :: previous_hash = '0000'
        character(len=256) :: root_merkle = '0000'
        character(len=256) :: hash = '0000'
        type(block), pointer :: next => null()
        type(b_avl), pointer :: branches => null()
    contains
        procedure :: generate_block
        procedure :: print_data
    end type block
    type chainer
        type(block), pointer :: head => null()
        type(block), pointer :: tail => null()
    contains 
        procedure :: add_block
        procedure :: print_chain
        procedure :: generate_files
        procedure :: chain_dot
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
    this%index = block_id
    block_id = block_id + 1
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
                    b_destination%place, current%next%distance*80)
                end if
            end if
        end if
        current => current%next
    end do
    call new_merkle%generate()
    call new_merkle%merkle_dot()
    this%root_merkle = new_merkle%get_head_hash()
    end subroutine generate_block
    subroutine print_data(this)
        class(block), intent(in) :: this
        
        print *, 'Index: ', this%index
        print *, 'Timestamp: ', this%timestamp
        print *, 'Nonce: ', this%nonce
        call this%data%print()
        print *, 'Previous Hash: ', this%previous_hash
        print *, 'Root Merkle: ', this%root_merkle
        print *, 'Hash: ', this%hash

        
    end subroutine print_data
    subroutine add_block(this,  new_block)
        class(chainer), intent(inout) :: this
        type(block), pointer, intent(inout) :: new_block
        character(len=100) :: index, nonce 

        write(index, '(I0)') new_block%index
        write(nonce, '(I0)') new_block%nonce
        if ( .not. associated(this%head) ) then
            this%head => new_block
            new_block%hash = sha256(trim(index)//new_block%timestamp//trim(nonce)&
            //new_block%previous_hash//new_block%root_merkle)
            this%tail => new_block
        else
            this%tail%next => new_block
            new_block%previous_hash = this%tail%hash
            new_block%hash = sha256(trim(index)//new_block%timestamp//trim(nonce)&
            //new_block%previous_hash//new_block%root_merkle)
            this%tail => new_block
        end if
    end subroutine add_block
    subroutine print_chain(this)
        class(chainer), intent(inout) :: this
        type(block), pointer :: current

        current => this%head
        do while ( associated(current) )
            call current%print_data()
            current => current%next
        end do
        
    end subroutine print_chain
    subroutine generate_files(this)
        class(chainer), intent(inout) :: this    
        type(json_core) :: json
        type(json_value), pointer :: p, data_p, path 
        type(block), pointer :: current
        type(result), pointer :: current_data
        type(branch), pointer :: b_actual
        type(branch), pointer :: b_next
        character(len=5) :: file_index
        current => this%head
        do while ( associated(current) )
            call json%initialize()
            call json%create_object(p, '')
            call json%add(p, 'INDEX', current%index)
            call json%add(p, 'TIMESTAMP', current%timestamp)
            call json%add(p, 'NONCE', current%nonce)
            ! Data
            call json%create_array(data_p, 'DATA')
            current_data => current%data%head
            do while ( associated(current_data) )
                b_actual => current%branches%search_branch(current_data%id)
                if ( associated(b_actual) ) then
                    if ( associated(current_data%next) ) then
                        b_next => current%branches%search_branch(current_data%next%id)
                        if ( associated(b_next) ) then
                            call json%create_object(path, '')
                            call json%add(path, 'sucursal_o', current_data%id)
                            call json%add(path, 'direccion_o', b_actual%place//', '//b_actual%address)
                            call json%add(path, 'sucursal_d', current_data%next%id)                            
                            call json%add(path, 'direccion_d', b_next%place//', '//b_next%address)
                            call json%add(path, 'costo', current_data%next%distance*80)
                            call json%add(data_p, path)
                        end if
                    end if
                end if
                current_data => current_data%next
            end do
            call json%add(p, data_p)
            call json%add(p, 'PREVIOUS_HASH', trim(current%previous_hash))
            call json%add(p, 'ROOT_MERKLE', trim(current%root_merkle))
            call json%add(p, 'HASH', trim(current%hash))
            nullify(data_p)
            nullify(path)  
            write(file_index, '(I0)') current%index
            call json%print(p, 'outputs/block_'//trim(file_index)//'.json')  
            print *, 'Block_'//trim(file_index)//' generated'
            current => current%next
        end do    
        call json%destroy(p)
    end subroutine generate_files
    subroutine chain_dot(this)
        class(chainer), intent(inout) :: this    
        type(block), pointer :: current

        current => this%head
        open(99, file='outputs/chain.dot', status='replace')
        write(99, '(A)') 'digraph G {'
        write(99, '(A)') 'rankdir=LR;'
        write(99, '(A)') 'node [shape=record];'
        do while ( associated(current) )
            write(99, '(A, I0, A)') 'block_', current%index, '[label=<<TABLE><TR>'
            write(99, '(A, I0, A)') '<TD>Index: ', current%index, '</TD></TR>'
            write(99, '(A, A, A)') '<TR><TD>Timestamp: ', current%timestamp, '</TD></TR>'
            write(99, '(A, I0, A)') '<TR><TD>Nonce: ', current%nonce, '</TD></TR>'
            write(99, '(A)') '<TR><TD>Previous Hash: '
            write(99, '(A)') trim(current%previous_hash)
            write(99, '(A)') '</TD></TR>'
            write(99, '(A)') '<TR><TD>Root Merkle: '
            write(99, '(A)') trim(current%root_merkle)
            write(99, '(A)') '</TD></TR>'
            write(99, '(A)') '<TR><TD>Hash: '
            write(99, '(A)') trim(current%hash)
            write(99, '(A)') '</TD>'
            write(99, '(A)') '</TR></TABLE>>];'
            if ( associated(current%next) ) then
                write(99, '(A, I0, A, I0, A)') 'block_', current%index, ' -> block_', current%next%index, ';'
            end if
            current => current%next
        end do
        write(99, '(A)') '}'
        close(99)
        call execute_command_line('dot -Tsvg outputs/chain.dot -o outputs/chain.svg')
    end subroutine chain_dot
end module block_chain