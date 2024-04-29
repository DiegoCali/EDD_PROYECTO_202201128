program merkle_test
    use merkle_tree
    implicit none
    type(merkle) :: tree 

    call tree%add_data(1, "Chimaltenango", 3, "Guatemala", 500)
    call tree%add_data(2, "Quetzaltenango", 3, "Guatemala", 200)
    call tree%add_data(3, "Huehuetenango", 1, "Chimaltenango", 100)
    call tree%add_data(4, "San Marcos", 2, "Quetzaltenango", 50)
    call tree%add_data(5, "Solola", 2, "Quetzaltenango", 150)

    call tree%generate()

    call tree%merkle_dot() 

end program merkle_test