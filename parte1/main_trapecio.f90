program parte1

use mod_prec
use trapecio

implicit none

  !Declaración de variables
	real(wp)							:: int
	real(wp), dimension(:), allocatable	:: x, y
	integer(il)							:: fu, i, cant_datos, st
	character(20)						:: archivo, line1, line2
 
  !Inicialización de variables 
	archivo = 'datos.dat'
	cant_datos = 0
			
  !Acá leemos la cantidad de datos
	open(newunit=fu, file=archivo, status='old', action='read')
		!st = 0
		do 
			read(fu,*,iostat=st) line1
			print *, line1
			if (st /= 0) exit
			cant_datos = cant_datos + 1
			
		end do
		
		print *, cant_datos
		
	close (fu)
  
  !Escribimos los datos en los vectores
	
	allocate (x(0:cant_datos-1), y(0:cant_datos-1))

	open(newunit=fu, file=archivo, status='old', action='read')
		
		do i = 0, cant_datos - 1	
			read(fu,*) x(i), y(i)		
			print *, x(i), y(i)
		end do
			
	close 	(fu)
	
	call trapecionoeq(x, y, cant_datos, int)
	
	print*, int
	
	
deallocate (x, y)

end program parte1
