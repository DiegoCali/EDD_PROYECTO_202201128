module images
    use layers
    implicit none
    type :: image
        integer :: id
        integer :: height
        integer :: layers_count = 0
        type(layers_tree) :: layers
        type(image), pointer :: left => null()
        type(image), pointer :: right => null()
    contains
        procedure :: add_layer
    end type image
    type :: image_avl
        type(image), pointer :: root => null()
    contains
        procedure :: add_img
        procedure :: add_img_rec
        procedure :: search_img
        procedure :: srl
        procedure :: srr
        procedure :: drl
        procedure :: drr
        procedure :: get_max
        procedure :: min_child
        procedure :: get_height
        procedure :: get_dot
        procedure :: get_dot_rec
        procedure :: delete_img
        procedure :: delete_img_rec
    end type image_avl
contains
    subroutine delete_img(this, img_id)
        class(image_avl), intent(inout) :: this
        integer, intent(in) :: img_id
        if ( associated(this%root) ) then
            this%root => this%delete_img_rec(this%root, img_id)
        else 
            print *, 'Trer is empty!'
        end if
    end subroutine delete_img
    recursive function delete_img_rec(this, temp, img_id) result(new_sub_tree)
        class(image_avl), intent(inout) :: this
        type(image), pointer, intent(inout) :: temp
        integer, intent(in) :: img_id
        type(image), pointer :: aux
        type(image), pointer :: new_sub_tree        
        if ( .not. associated(temp) ) then
            new_sub_tree => null()
            return
        end if

        if ( img_id < temp%id ) then
            temp%left => this%delete_img_rec(temp%left, img_id)
        else if ( img_id > temp%id ) then
            temp%right => this%delete_img_rec(temp%right, img_id)
        else 
            if ( .not.associated(temp%left) .or. .not.associated(temp%right) ) then
                aux => null()
                if ( associated(temp%left) ) then
                    aux => temp%left
                else 
                    aux => temp%right
                end if
                if ( .not.associated(aux) ) then
                    aux => temp
                    temp => null()
                else
                    temp => aux
                end if
            else
                aux => this%min_child(temp%right)
                temp%id = aux%id
                temp%layers_count = aux%layers_count
                temp%layers = aux%layers
                temp%right => this%delete_img_rec(temp%right, aux%id)
            end if
        end if
        if ( .not.associated(temp) ) then
            new_sub_tree => temp
            return
        end if
        if ( (this%get_height(temp%left) - this%get_height(temp%right)) == 2 ) then
            if ( this%get_height(temp%left%left) - this%get_height(temp%left%right) == 1 ) then
                temp => this%srl(temp)
            else
                temp => this%drl(temp)
            end if
        else if ( (this%get_height(temp%right) - this%get_height(temp%left)) == 2 ) then
            if ( this%get_height(temp%right%right) - this%get_height(temp%right%left) == 1 ) then
                temp => this%srr(temp)
            else
                temp => this%drr(temp)
            end if
        end if
        temp%height = this%get_max(this%get_height(temp%left), this%get_height(temp%right)) + 1
        new_sub_tree => temp
    end function delete_img_rec
    recursive function min_child(this, temp) result(minimal)
        class(image_avl), intent(inout) :: this
        type(image), pointer, intent(in) :: temp
        type(image), pointer :: minimal
        minimal => temp
        do while ( associated(minimal%left) )
            minimal => minimal%left
        end do    
    end function min_child
    subroutine add_layer(this, new_layer)
        class(image), intent(inout) :: this
        type(layer), pointer, intent(in) :: new_layer
        call this%layers%add_copied_val(new_layer)
        this%layers_count = this%layers_count + 1
    end subroutine add_layer
    subroutine add_img(this,  new_image)
        class(image_avl), intent(inout) :: this
        type(image), pointer, intent(in) :: new_image
        if ( associated(this%root) ) then
            call this%add_img_rec(new_image, this%root)
        else
            this%root => new_image
        end if
    end subroutine add_img
    subroutine add_img_rec(this,  new_image, tmp)
        class(image_avl), intent(inout) :: this
        type(image), pointer, intent(in) :: new_image
        type(image), pointer :: tmp
        integer :: r, l, m
        if ( .NOT. associated(tmp) ) then
            tmp => new_image
        else if (new_image%id < tmp%id) then
            call this%add_img_rec(new_image, tmp%left)
            if ( (this%get_height(tmp%left) - this%get_height(tmp%right)) == 2 ) then
                if ( new_image%id < tmp%left%id ) then
                    tmp => this%srl(tmp)
                else
                    tmp => this%drl(tmp)
                end if
            end if
        else 
            call this%add_img_rec(new_image, tmp%right)
            if ( (this%get_height(tmp%right) - this%get_height(tmp%left)) == 2 ) then
                if ( new_image%id > tmp%right%id ) then
                    tmp => this%srr(tmp)
                else
                    tmp => this%drr(tmp)
                end if
            end if
        end if
        r = this%get_height(tmp%right)
        l = this%get_height(tmp%left)
        m = this%get_max(r, l)
        tmp%height = m + 1        
    end subroutine add_img_rec
    recursive function search_img(this, temp, img_ig) result(img_pointer)
        class(image_avl), intent(in) :: this
        integer, intent(in) :: img_ig
        type(image), pointer, intent(in) :: temp
        type(image), pointer :: img_pointer
        if ( .not. associated(temp) ) then
            img_pointer => null()
            return
        end if
        if ( img_ig == temp%id ) then
            img_pointer => temp
        else if ( img_ig < temp%id ) then
            img_pointer => this%search_img(temp%left, img_ig)
        else
            img_pointer => this%search_img(temp%right, img_ig)
        end if
    end function search_img
    function srl(this, t1) result(t2)
        class(image_avl), intent(in) :: this
        type(image), pointer, intent(in) :: t1
        type(image), pointer :: t2
        t2 => t1%left
        t1%left => t2%right
        t2%right => t1
        t1%height = this%get_max(this%get_height(t1%left), this%get_height(t1%right)) + 1
        t2%height = this%get_max(this%get_height(t2%left), t1%height) + 1
    end function srl
    function srr(this, t1) result(t2)
        class(image_avl), intent(in) :: this
        type(image), pointer, intent(in) :: t1
        type(image), pointer :: t2
        t2 => t1%right
        t1%right => t2%left
        t2%left => t1
        t1%height = this%get_max(this%get_height(t1%left), this%get_height(t1%right)) + 1
        t2%height = this%get_max(this%get_height(t2%right), t1%height) + 1
    end function srr
    function drl(this, tmp) result(res)
        class(image_avl), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        type(image), pointer :: res
        tmp%left => this%srr(tmp%left)
        res => this%srl(tmp)        
    end function drl
    function drr(this, tmp) result(res)
        class(image_avl), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        type(image), pointer :: res
        tmp%right => this%srl(tmp%right)
        res => this%srr(tmp)        
    end function drr
    function get_max(this, val1, val2) result(res)
        class(image_avl), intent(in) :: this
        integer, intent(in) :: val1, val2 
        integer :: res
        res = merge(val1, val2, val1 > val2)   
    end function get_max
    function get_height(this, tmp) result(res)
        class(image_avl), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        integer :: res 
        if ( .NOT. associated(tmp) ) then
            res = -1
        else
            res = tmp%height
        end if   
    end function get_height
    subroutine get_dot(this, tmp, unit)
        class(image_avl), intent(in) :: this
        type(image), pointer, intent(in) ::  tmp
        integer, intent(in) ::  unit
        write(unit, '(A)') 'digraph image_avl {'
        call this%get_dot_rec(tmp, unit)
        write(unit, '(A)') '}'
    end subroutine get_dot
    subroutine get_dot_rec(this, tmp, unit)
        class(image_avl), intent(in) :: this
        type(image), pointer, intent(in) ::  tmp
        integer, intent(in) ::  unit
        if ( .NOT. associated(tmp) ) then
            return
        end if
        write(unit, '(A, I0, A, I0, A)') 'node_', tmp%id, '[label="img_', tmp%id, '"];'        
        if ( associated(tmp%left) ) then
            write(unit, '(A, I0, A, I0, A)') 'node_', tmp%id, ' -> node_', tmp%left%id, ';'
        end if
        if ( associated(tmp%right) ) then
            write(unit, '(A, I0, A, I0, A)') 'node_', tmp%id, ' -> node_', tmp%right%id, ';'
        end if
        call this%get_dot_rec(tmp%left, unit)
        call this%get_dot_rec(tmp%right, unit)
    end subroutine get_dot_rec
end module images