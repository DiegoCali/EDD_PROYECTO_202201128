program main
  use EDD_PROYECT_202201128, only: say_hello
  use randomizer
  implicit none
  logical          :: running
  integer          :: option
  type(rand)       :: randit

  randit%names = (/"Diego", "Carly", "Anaya", "Clint", "Ethan"/)
  randit%last_names = (/"Smith", "Brown", "Clark", "White", "Moore"/)
  running = .TRUE.
  call say_hello()
  do while(running)
    print *, "PLEASE SELECT AN OPTION:"
    print *, "1. Initial parameters"
    print *, "2. Execute Step"
    print *, "3. Structures memory state"
    print *, "4. Reports"
    print *, "5. About me"
    print *, "6. Exit"
    read *, option
    select case (option)
      case (1)
        print *, "Nothing yet"
      case (2)
        print *, "Nothing yet"
      case (3)
        print *, "Nothing yet"
      case (4)
        print *, "Nothing yet"
      case (5)
        print *, "----------------------------------------"
        print *, "  Diego Felipe Cali Morales: 202201128"
        print *, "----------------------------------------"
      case (6)
        running = .FALSE.
    end select
  end do
  print *, "//----------Program terminated----------\\"
end program main
