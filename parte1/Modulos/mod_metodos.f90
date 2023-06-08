module trapecio

! Mercado Alcoba, Javier
! Sollender, Jazmín

use mod_prec 

implicit none

contains

	subroutine trapecionoeq (x, y, n, int)
		
		use mod_prec
		
		implicit none
	
	!Declaracion de variables
		real(wp), dimension(0:n), intent(in)    :: x, y
		real(wp), intent(out)		          :: int
		integer(il), intent(in)		          :: n
		 
	!Declaracion de variables aux
		real(wp)  		        :: aux, h, xi
		integer(il)			    :: i
		
	!Incializacion de variables
		
		int = 0.0_wp	
		
	!Bloque de procesamiento
	
		do i = 0 , n-1
						
			h = x(i+1)-x(i)
			aux = y(i) + y(i+1)/2
			int = int + aux * h
		    
		end do 
		
		print *, ''
		print *, '	La integral obtenida utilizando el método del trapecio para puntos no quiespaciados es:', int
		print *, ''
		print *, '  () ()  |/'
		print *, ' ( · .·) '
		print *, '*(  u u)'
		
	end subroutine trapecionoeq
	
end module trapecio
