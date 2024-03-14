program check
	use pixels
	implicit none
	type(pixel_matrix) :: matrix 

	call matrix%insert(1,3,.TRUE.)	
	call matrix%insert(5,2,.TRUE.)	
	call matrix%insert(3,3,.TRUE.)	
	call matrix%insert(4,6,.TRUE.)	
	call matrix%self_print()

	open(1, file='pixels.dot', status='replace')
	write(1,'(A)') 'digraph Layer1 {'
	call matrix%graph_pixels(1)
	write(1,'(A)') '}'
end program check
