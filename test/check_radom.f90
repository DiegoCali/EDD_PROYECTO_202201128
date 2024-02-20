program random_test
    use randomizer
    implicit none
    type(rand)        :: random
    integer           :: rnumber    
    character(len=11) :: rname
    
    random%names = (/"Diego", "Carly", "Anaya", "Clint", "Ethan"/)
    random%last_names = (/"Smith", "Brown", "Clark", "White", "Moore"/)
    rname = random%get_rand_name()
    rnumber = random%get_rand_number(0, 3)
    print *, rname, rnumber
end program random_test