module metodos
use mod_prec
use funciones
implicit none 

contains
!############################################################################################
!························METODOS DE INTEGRACION NUMERICA MODIFICADOS·························
!############################################################################################

!###########################################################################################
!----------------METODO DE TRAPECIO PARA PUNTOS NO EQUDISTANTES-----------------------------
!------------------------------------------------------------------------------------------- 
    subroutine trapecionoeq (x, y, n, int)
    !Declaracion de variables
    integer(il), intent(in)                 :: n    !cantidad de datos
    real(wp), dimension(0:n), intent(in)    :: x, y !vectores con datos (posicion y velocidad)
    real(wp), intent(out)                   :: int  !integral que escupe la subrutina
    
    !Declaracion de variables aux
    real(wp)                                :: aux, h, xi
    integer(il)                             :: i
    
    !Incializacion de variables
    int = 0.0_wp

    !Bloque de procesamiento
    
        do i = 0 , n-1
            h = x(i+1)-x(i)
            aux = y(i) + y(i+1)/2
            int = int + aux * h
        end do 
    
        print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
        !print *, 'la integral es', int
        print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    
    end subroutine trapecionoeq
    

!###########################################################################################
!---------------------------METODO DE SIMPSON COMPUESTO-------------------------------------
!------------------------------------------------------------------------------------------- 

	subroutine simpson (a, b, n, f, int)
	    
      !Declaracion de variables
      	real(wp), intent(in)	 :: a, b    !intervalo 
      	real(wp)				 :: f       !funcion
      	integer(il), intent (in) :: n       !cant puntos (par)
      	real (wp), intent(out)	 :: int     !integrak
      !Declaracion de variables auxiliares
      	real(wp)				 :: h, x_par, x_impar    !intervalo, puntos
      	integer(il)				 :: i       !los pasitos
      	
      !Inicialización de variables
      
      	h = (b-a)/n
      	int = (f(a) + f(b))/3._wp
      	
      	par: do i = 1 , (n/2)-1, 1
            x_par = a + 2._wp*i*h
      	    !x_par = a + 2._wp*i*h + h
      		int = int + (2._wp/3)*f(x_par)
      	end do par
      	
      	impar: do i = 1 , (n/2), 1
            x_impar = a +  (2._wp*i - 1) * h
      	    !x_impar = a + 2._wp*i*h
      		int = int + (4._wp/3)*f(x_impar)
      	end do impar
      	
      	int = int * h
      	
	end subroutine simpson


!############################################################################################
!························METODOS DE DERIVACION NUMERICA MODIFICADOS··························
!############################################################################################


!###########################################################################################
!-------------------------FORMULA 3 PUNTOS CENTRADO-----------------------------------------
!-------------------------------------------------------------------------------------------	
	
	subroutine derivada3centrada(x, h, y, n, dfc)
	
	!Declaracion de Dummy varaibles
        integer(il), intent(in)              :: n
		real(wp), intent(in), dimension(0:n) :: x, y 
        real(wp), intent(in)                 :: h
        real(wp), intent(out)                :: dfc
        
        dfc = (1._wp/(2_wp*h)) * (y(n) - y(0))    
	    !dfc = 1._wp/2_wp*h * (f(c + h) - f(c - h))
	
	end subroutine derivada3centrada

!############################################################################################
!-------------------------FORMULA 5 PUNTOS CENTRADO-----------------------------------------
!-------------------------------------------------------------------------------------------	
	
	subroutine derivada5centrada(x, h, y, n, dfc)

    !Declaracion de Dummy variables
        integer(il), intent(in)              :: n
        real(wp), intent(in), dimension(0:n) :: x, y
        real(wp), intent(in)                 :: h
        real(wp), intent(out)                :: dfc
	    
	    !Declaracion de variables auxiliares
        dfc = (1._wp/(12._wp*h))*(y(0) - 8._wp*y(n-3) + 8._wp*y(n-1) - y(n))

	   ! dfc = (1._wp/(12._wp*h))*(f(c - 2._wp*h) - 8._wp*f(c-h) + 8._wp*f(c + h) - f(c + 2._wp*h))  
	   !dfc = 1._wp/(12._wp*h)*(f(c - 2._wp*h) - 8._wp*(c-h) + 8._wp*(c + h) - f(c + 2._wp*h))	
	    
	end subroutine derivada5centrada

!##############################################################################################
!-------------------------FORMULA 2 PUNTOS HACIA ADELANTE-------------------------------------
!---------------------------------------------------------------------------------------------

	subroutine derivada2adelante(x, h, y, n, dfc)
	!Declaracion de Dummy variables
        integer(il), intent(in)              :: n
        real(wp), intent(in), dimension(0:n) :: x, y
        real(wp), intent(in)                 :: h
        real(wp), intent(out)                :: dfc
        
        dfc = (y(n) - y(0)) / h
        !dfc = (f(c+h)-f(c))/h
	    
	end subroutine derivada2adelante

!##############################################################################################
!-------------------------FORMULA 2 PUNTOS HACIA ATRAS-----------------------------------------
!----------------------------------------------------------------------------------------------

	subroutine derivada2atras (x, h, y, n, dfc)
	!Declaracion de Dummy variables
        integer(il), intent(in)              :: n
        real(wp), intent(in), dimension(0:n) :: x, y
        real(wp), intent(in)                 :: h
        real(wp), intent(out)                :: dfc

	    dfc = (y(n) - y(0)) / h
	    !dfc = (f(c)-f(c-h))/h
	    
	end subroutine derivada2atras
	
!############################################################################################
!························METODOS DE INTERPOLACION POLINOMIAL··························
!############################################################################################
	subroutine lagrange(n, x, fx, c, pc)
        integer(il), intent(in)                  ::  n  !grado del polinomio
        real(wp), dimension(0:n), intent(in)     :: x  !vector de los xi
        real(wp), dimension(0:n), intent(in)     :: fx !vector de los fxi
        real(wp), intent(in)                     :: c   !punto a evaluar
        real(wp), intent(out)                    :: pc  !polinomio evaluado c
        
        !variables auxiliares
        real(wp)                    :: lci
        integer(il)                 :: i, k
        !-----------------------------------------------------
        !Proceso
        pc = 0.0_wp
        !auxl = (c - x(i)) / (x(k)- x(i))
        do k = 0, n
            lci = 1.0_wp      
            do i = 0, n
                if (k /= i) then
                    lci = lci * (c - x(i)) / (x(k)- x(i))                     
                end if
                
            end do
            pc = pc + lci * fx(k)
        end do
        
    end subroutine lagrange

end module metodos
