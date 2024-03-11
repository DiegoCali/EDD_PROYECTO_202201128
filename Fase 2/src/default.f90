module default
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, default!"
  end subroutine say_hello
end module default
