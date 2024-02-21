program client_queue_test
    use client_queue
    use image_stack
    implicit none
    type(queue) :: new_queue
    type(stack) :: stack_1
    type(stack) :: stack_2
    type(stack) :: stack_3

    ! Preparing stacks
    call stack_1%push(2)
    call stack_1%push(1)
    call stack_1%push(2)
    
    call stack_2%push(1)
    call stack_2%push(1)
    call stack_2%push(2)

    call stack_3%push(1)
    call stack_3%push(2)
    call stack_3%push(2)

    ! Preparing clients
    call new_queue%enqueue("Different", stack_1)
    call new_queue%enqueue("Lenght", stack_2)
    call new_queue%enqueue("Names Of People", stack_3)

    ! Showing queue
    call new_queue%self_print()
end program client_queue_test