module randomizer
    implicit none            

    type rand
        character(len=5), dimension(5) :: names
        character(len=5), dimension(5) :: last_names
    contains
        procedure :: get_rand_name
        procedure :: get_rand_number
    end type rand
contains
    function get_rand_name(this) result(result)
        class(rand), intent(inout) :: this
        character(len=11) :: result
        character(len=5) :: name
        character(len=5) :: last
        integer :: number_1
        integer :: number_2
        number_1 = this%get_rand_number(1, 4)
        number_2 = this%get_rand_number(1, 4)
        name = this%names(number_1)
        last = this%last_names(number_2)
        result = name // " " // last
    end function
    function get_rand_number(this, n, m) result(number)
        class(rand), intent(inout) :: this
        integer, intent(in) :: n, m
        integer :: number
        real :: u
        call random_number(u)
        number = n + FLOOR((m+1)*u)
    end function
end module randomizer