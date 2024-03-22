module albums
    use images, only: image
    implicit none
    integer :: album_id = 0
    type :: img_node
        integer :: id
        type(image), pointer :: img_pointer => null()
        type(img_node), pointer :: next => null()
    end type img_node
    type :: album
        integer :: id
        character(:), allocatable :: name
        type(album), pointer :: next => null()
        type(album), pointer :: prev => null()
        type(img_node), pointer :: head => null()
        type(img_node), pointer :: tail => null()
        integer :: size = 0
    contains
        procedure :: add_image
        procedure :: remove_image
        procedure :: get_image
        procedure :: show_images
    end type album
    type :: album_list
        type(album), pointer :: head => null()
        type(album), pointer :: tail => null()
        integer :: size = 0
    contains
            procedure :: new_album
            procedure :: remove_album
            procedure :: get_album
            procedure :: search_in_album            
            procedure :: add_image_to_album
            procedure :: remove_image_from_album
            procedure :: show_albums
            procedure :: show_album_images
    end type album_list
contains
    subroutine show_album_images(this, id_album)
        class(album_list), intent(in) :: this
        integer, intent(in) :: id_album
        type(album), pointer :: current
        current => this%get_album(id_album)
        if ( .not. associated(current) ) then
            write (*, '(A, I0, A)') "Album :", id_album, " not found..."
            return
        end if
        write (*, '(A, A)') "Album: ", current%name
        call current%show_images()
    end subroutine show_album_images
    subroutine show_albums(this)
        class(album_list), intent(in) :: this
        type(album), pointer :: current
        current => this%head
        if ( .not. associated(current) ) then
            write (*, '(A)') "No albums found..."
            return
        end if
        do while (associated(current))
            write (*, '(I0, A, A, A, I0, A)') current%id, ". ", current%name, ' (', current%size, ')'
            current => current%next
        end do    
    end subroutine show_albums
    subroutine remove_image_from_album(this, ralbum_id, image_id)
        class(album_list), intent(inout) :: this
        integer, intent(in) :: ralbum_id
        integer, intent(in) :: image_id
        type(album), pointer :: searched_album
        searched_album => this%get_album(ralbum_id)
        if ( .not. associated(searched_album) ) then
            write (*, '(A, I0, A)') "Album :", ralbum_id, " not found..."
            return
        end if
        call searched_album%remove_image(image_id)        
    end subroutine remove_image_from_album
    subroutine add_image_to_album(this, ialbum_id, image_node)
        class(album_list), intent(inout) :: this
        integer, intent(in) :: ialbum_id
        type(image), pointer, intent(in) :: image_node
        type(album), pointer :: searched_album 
        searched_album => this%get_album(ialbum_id)
        if ( .not. associated(searched_album) ) then
            write (*, '(A, I0, A)') "Album :", ialbum_id, " not found..."
            return
        end if
        call searched_album%add_image(image_node)        
    end subroutine add_image_to_album
    function search_in_album(this, salbum_id, image_id) result(return_image)
        class(album_list), intent(in) :: this
        integer, intent(in) :: salbum_id
        integer, intent(in) :: image_id
        type(image), pointer :: return_image
        type(album), pointer :: searched_album
        searched_album => this%get_album(salbum_id)
        if ( .not. associated(searched_album) ) then
            return_image => null()
            return
        end if
        return_image => searched_album%get_image(image_id)
    end function search_in_album
    function get_album(this, id) result(result_album)
        class(album_list), intent(in) :: this
        integer, intent(in) :: id
        type(album), pointer :: current
        type(album), pointer :: result_album
        result_album => null()
        current => this%head
        if ( .not. associated(current) ) then
            return
        end if
        do while (associated(current))
            if ( current%id == id ) then
                result_album => current
                return
            end if
            current => current%next
        end do
    end function get_album
    subroutine remove_album(this, id)
        class(album_list), intent(inout) :: this
        integer, intent(in) :: id
        type(album), pointer :: current
        current => this%head
        if (.not. associated(current)) return
        if ( current%id == id ) then
            if ( this%head%id == this%tail%id ) then
                this%head => null()
                this%tail => null()
            else 
                this%head => current%next
            end if
            deallocate(current)
            return
        else
            do while (associated(current))
                if ( current%id == id ) then
                    if (associated(current%prev)) then
                        current%prev%next => current%next
                    end if
                    if (associated(current%next)) then
                        current%next%prev => current%prev
                    end if
                    if ( current%id == this%tail%id ) then
                        this%tail => current%prev
                    end if
                    deallocate(current)
                    write (*, '(A, I0, A)') "Album :", id, " removed..."
                    return
                end if
                current => current%next
            end do
        end if
    end subroutine remove_album
    subroutine new_album(this, album_name)
        class(album_list), intent(inout) :: this
        character(*), intent(in) :: album_name
        type(album), pointer :: create_album
        allocate(create_album)
        create_album%id = album_id
        album_id = album_id + 1
        create_album%name = album_name
        if (this%size == 0) then
            this%head => create_album
            this%tail => create_album
        else
            this%tail%next => create_album
            create_album%prev => this%tail
            this%tail => create_album
        end if
        this%size = this%size + 1        
    end subroutine new_album
    subroutine add_image(this,  image_node)
        class(album), intent(inout) :: this
        type(image), pointer, intent(in) :: image_node
        type(img_node), pointer :: new_node
        allocate(new_node)
        new_node%id = image_node%id
        new_node%img_pointer => image_node
        if (this%size == 0) then
            this%head => new_node
            this%tail => new_node
        else
            this%tail%next => new_node
            this%tail => new_node
        end if
        this%size = this%size + 1
    end subroutine add_image
    subroutine remove_image(this, id)
        class(album), intent(inout) :: this
        integer, intent(in) :: id    
        type(img_node), pointer :: current
        type(img_node), pointer :: previous
        current => this%head
        if (.not. associated(current)) return
        if ( current%id == id ) then
            if ( this%head%id == this%tail%id ) then
                this%head => null()
                this%tail => null()
            else 
                this%head => current%next
            end if
            deallocate(current)
            return
        else 
            previous => current
            current => current%next
        end if
        do while (associated(current))
            if ( current%id == id ) then
                if ( current%id == this%tail%id ) then
                    this%tail => previous
                end if
                previous%next => current%next
                deallocate(current)
                write (*, '(A, I0, A)') "Image :", id, " removed..."
                return
            end if
            previous => current
            current => current%next
        end do
    end subroutine remove_image
    function get_image(this, id) result(return_image)
        class(album), intent(in) :: this
        integer, intent(in) :: id
        type(img_node), pointer :: current
        type(image), pointer :: return_image    
        if ( .NOT. associated(current) ) then
            return_image => null()
            return
        end if
        current => this%head
        do while (associated(current))
            if ( current%id == id ) then
                return_image => current%img_pointer
                return
            end if
            current => current%next
        end do
        return_image => null()
        print *, "Image not found..."
    end function get_image
    subroutine show_images(this)
        class(album), intent(in) :: this
        type(img_node), pointer :: current
        current => this%head
        if ( .not. associated(current) ) then
            write (*, '(A)') "No images found..."
            return
        end if
        do while (associated(current))
            write (*, '(I0, A, I0, A)') current%id, '. img (', current%img_pointer%layers_count, ')'
            current => current%next
        end do
        
    end subroutine show_images
end module albums