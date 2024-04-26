program sha_test
    use sha256_module
    implicit none
    character(len=256) :: output1, output2, output3, input3
    integer :: number = 1234567890

    output1 = sha256("Diego"//"Felipe"//"5tas San Jorge"//"Chimaltenango")
    output2 = sha256("DiegoFelipe5tas San JorgeChimaltenango0")

    !Parse number to character with read
    write(input3, '(I10)') number
    print *, "DiegoFelipe5tas San JorgeChimaltenango"//input3
    output3 = sha256("DiegoFelipe5tas San JorgeChimaltenango"//input3)

    print *, output1
    print *, output2
    print *, output3
end program sha_test