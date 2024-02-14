program json_string_test
    use :: json_module, cdk => json_CDK
    implicit none
    character(kind=cdk,len=:), allocatable :: message
    type(json_file)           :: json
    logical                   :: is_found
    
    call json%initialize()

    call json%load_file('config.json'); if (json%failed()) stop

    json_block: block
        call json%get("text", message, is_found); if (.not. is_found) exit json_block
    end block json_block

    if (is_found) then
        print *, "JSON TEXT RETRIEVE TEST:"
        print *, message
    end if

    call json%destroy()
    print *, "FINISHED STRING JSON TEST"
end program json_string_test