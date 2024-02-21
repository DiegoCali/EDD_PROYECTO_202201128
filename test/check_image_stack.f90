program check
    use image_stack
    implicit none
    type(stack) :: new_stack
    type(stack) :: aux_stack

    print *, "ADD TO STACK"
    call new_stack%push(1)
    call new_stack%push(2)
    call new_stack%push(3)
    call new_stack%push(4)

    print *, "POP STACK"
    call aux_stack%push_node(new_stack%pop())
    call aux_stack%push_node(new_stack%pop())

    print *, "PRINT STACK"
    call new_stack%self_print()
    call aux_stack%self_print()

    print *, "FINISHED STACK TEST"
end program check
