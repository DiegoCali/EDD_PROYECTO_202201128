program check
	use pixels
	implicit none
	type(pixel_matrix) :: matrix 

	call matrix%insert(1,3,.TRUE., "#FF0000")	
	call matrix%insert(5,2,.TRUE., "#00FF00")	
	call matrix%insert(3,3,.TRUE., "#0000FF")	
	call matrix%insert(4,6,.TRUE., "#FFFF00")	
	call matrix%insert(2,4,.TRUE., "#00FFFF")
	call matrix%insert(6,5,.TRUE., "#FF00FF")
	call matrix%insert(7,7,.TRUE., "#000000")
	! call matrix%self_print()

	open(1, file='pixels.dot', status='replace')
	write(1,'(A)') 'digraph Layer1 {'
	call matrix%graph_pixels(1)
	write(1,'(A)') '}'
	close(1)
	print *, 'File made successfully!'
	call execute_command_line('dot -Tsvg pixels.dot > out_pixel.svg')
end program check
