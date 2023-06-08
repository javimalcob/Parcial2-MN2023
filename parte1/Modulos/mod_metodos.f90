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
		
		print*,''	
		print*,'Punto nro:	Integral punto a punto:'

		do i = 0 , n-1
						
			h = x(i+1)-x(i)
			aux = y(i) + y(i+1)/2
			int = int + aux * h
		    write(*,'(I6, 7X, F22.14)') i+1, int
		end do 
		
		print *, ''
		print *, '	La integral obtenida utilizando el método del trapecio para puntos no quiespaciados es:', int
		print *, ''
		print *, '  () ()  |/'
		print *, ' ( · .·) '
		print *, '*(  u u)'
		
	end subroutine trapecionoeq
	
end module trapecio
