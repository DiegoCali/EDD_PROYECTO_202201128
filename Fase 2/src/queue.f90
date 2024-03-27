module queue_mod
    use layers, only: layer
    implicit none
    type node_layer
        type(layer), pointer :: value => null()
        type(node_layer), pointer :: next => null()
    end type node_layer
    type queue
        type(node_layer), pointer :: head => null()
    contains 
        procedure :: enqueue
        procedure :: dequeue
        procedure :: is_empty
    end type queue
contains
    subroutine enqueue(this, layer_val)
        class(queue), intent(inout) :: this
        type(layer), pointer, intent(in) :: layer_val
        type(node_layer), pointer :: temp, new_node
        if (associated(this%head)) then
            allocate(new_node)
            new_node%value => layer_val
            new_node%next => null()
            temp => this%head
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => new_node
        else
            allocate(this%head)
            new_node => this%head
        end if
    end subroutine enqueue
    function dequeue(this) result(layer_val)
        class(queue), intent(inout) :: this
        type(layer), pointer :: layer_val
        type(node_layer), pointer :: temp
        if (associated(this%head)) then
            layer_val => this%head%value
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
        else
            layer_val => null()
        end if
    end function dequeue
    function is_empty(this) result(res)
        class(queue), intent(in) :: this
        logical :: res
        res = .not.associated(this%head)
    end function is_empty
end module queue_mod