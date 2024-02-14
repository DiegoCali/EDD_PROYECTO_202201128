program check
    use image_pile
    implicit none
    type(stack) :: new_stack

    print *, "ADD TO STACK"
    call new_stack%push("A")
    call new_stack%push("B")
    call new_stack%push("C")
    call new_stack%push("D")

    print *, "POP STACK"
    call new_stack%pop()

    print *, "PRINT STACK"
    call new_stack%self_print()

    print *, "FINISHED STACK TEST"
end program check
