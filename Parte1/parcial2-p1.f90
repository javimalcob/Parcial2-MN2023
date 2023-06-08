program parte1

! Mercado Alcoba, Javier
! Sollender, Jazmín

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
	
  !Encabezado
  	print*,'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
  	print*,'	Métodos Numéricos 2023 - Parcial 2 - Parte 1'
  	print*,'	         Mercado Alcoba,  Sollender         '
  	print*,'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'	
	print*,''
			
  !Acá leemos la cantidad de datos
	open(newunit=fu, file=archivo, status='old', action='read')
		!st = 0
		do 
			read(fu,*,iostat=st) line1
			!print *, line1
			if (st /= 0) exit
			cant_datos = cant_datos + 1
			
		end do
		
		print *,'La cantidad de datos que tenemos en el archivo datos.dat es:', cant_datos
		print*,''
		
	close (fu)
  
  !Escribimos los datos en los vectores
	
	allocate (x(0:cant_datos-1), y(0:cant_datos-1))

	open(newunit=fu, file=archivo, status='old', action='read')
			print*,'Los vectores		x		y'
		do i = 0, cant_datos-1	
			read(fu,*) x(i), y(i)		
			write(*,'(8X, 2F22.14)') x(i), y(i)
		end do
			
	close (fu)

  !Ahora llamamos a la subrutina, la evaluamos, y calculamos la integral
	call trapecionoeq(x, y, cant_datos-1, int)

deallocate (x, y)

end program parte1
