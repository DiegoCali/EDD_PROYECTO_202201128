program hash_test
    use tech_hash
    implicit none
    type(hash) :: table

    call table%init(7, 30, 70)

    call table%insert(tech(2897315340401_8, "Diego", "Cali", "Masculino", "Chimaltenango", "5342-1548"))
    call table%insert(tech(1234325340402_8, "Juan", "Cali", "Masculino", "Chimaltenango", "5342-1548"))
    call table%insert(tech(2897319816273_8, "Pedro", "Morales", "Masculino", "Chimaltenango", "5342-1548"))
    call table%insert(tech(8657421340404_8, "Maria", "Castillo", "Femenino", "Chimaltenango", "5342-1548"))
    call table%show()
    print *, ""
    call table%insert(tech(6541315340401_8, "Sebastian", "Acuta", "Masculino", "Chimaltenango", "5342-1548"))
    call table%insert(tech(3524154640401_8, "Roberto", "Aguirre", "Masculino", "Chimaltenango", "5342-1548"))    
    call table%show()
    print *, ""
end program hash_test