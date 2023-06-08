program parte2

!Mercado Alcoba, Javier
!Sollender, Jazmín

use mod_prec
use metodos
use funciones
implicit none


    !########################### ENTRADA DE DATOS################################
    !----------------------------------------------------------------------------
    !Seccion de Declaracion de variables
    real(wp)                                       :: c, h, dfc, int, a , b, m , alfa, g, work, pc
    real(wp), dimension(:), allocatable            :: t, z, v
    real(wp), dimension(:), allocatable            :: x, y , fz ! vectores auxiliares
    integer(il)                                    :: fu, i, std, nlines, n
    character(80)                                  :: archivo_out , archivo_in, line
    !----------------------------------------------------------------------------
    !Seccion para inicializar las variables del programa
    archivo_in = 'datos.dat'
    archivo_out = 'salida.dat'
    nlines = 0.0_wp
    
	print*,''
	print*,'#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#'
	print*,'	PARTE 2 - MN2023 - Mercado Alcoba, Sollender' 		
	print*,'#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#'
    print*,''
    print*,''    
    !-----------------------------------------------------------------------------
    !Abrimos el archivo de datos.dat para sacar los vectores t , z 
    !Primero averiguamos la cantidad de filas del archivo

    open(newunit=fu, file=archivo_in, status='old', action='read')
        do
            read(fu, *, iostat=std) line
            if (std /= 0) exit
            nlines = nlines + 1
        end do
    close(fu)    

    !Conocida la dimension en nlines le asignamos la cantidad de memoria incluido v
    allocate(t(0:nlines-1), z(0:nlines-1), v(0:nlines-1)) 
   
    !Cargamos los datos.dat al los vectores t , z
    open(newunit=fu, file=archivo_in, status='old', action='read')
    
        do i = 0, nlines-1
            read(fu,*) t(i), z(i)
        end do
        
    close(fu)


!############################## PROCESAMIENTO DE DATOS #######################################
    
    !Para calcular la velocidad usamos formulas de derivacion numerica 2, 3 y 5 puntos modificadas
    !y guardamos los resultados en salida.dat con 3 columnas   
    
	print*,''
	print*,'2B.	Cálculo de velocidades a través de métodos de integración numérica.'
	print*,'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    print*,''

    open(newunit=fu, file=archivo_out)
       
        write(fu,*)  "  t(i)                ", "        z(i)         ", "             v(i)"
       
        do i = 0, nlines-1
            h = 1.0_wp  !Longitud del intervalo tiempo equiespaciados 
  
       ! Primer punto uso formula de 2 puntos hacia adelante
            if (i == 0) then
                n = size(t(i:i+1)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i:i+1) 
                y = z(i:i+1)
                call derivada2adelante(x, h, y, n, dfc)
                v(i) = dfc
                
                deallocate(x,y)

       ! Segundo punto uso formula de 3 puntos centrada  
            else if (i == 1) then
                n = size(t(i-1:i+1)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-1:i+1)
                y = z(i-1:i+1)
                call derivada3centrada(x, h, y, n, dfc)
                v(i) = dfc
                
                deallocate(x,y)
  
       ! Anteultimo punto uso formula de 3 puntos centrada 
            else if (i == nlines-2) then
                n = size(t(i-1:i+1)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-1:i+1)
                y = z(i-1:i+1)
                call derivada3centrada(x, h, y, n, dfc)
                v(i) = dfc
                
                deallocate(x,y)
  
       ! Ultimo punto uso formula de 2 puntos hacia atras
            else if (i == nlines-1) then
                n = size(t(i-1:i)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-1:i)
                y = z(i-1:i)
                call derivada2atras(x, h, y, n, dfc)
                v(i) = dfc

                deallocate(x,y)
  
       !Puntos intermedios uso formula de 5 puntos centrada
            else
                n = size(t(i-2:i+2)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-2:i+2)
                y = z(i-2:i+2)
                call derivada5centrada(x, h, y, n, dfc)
                v(i) = dfc

                deallocate(x,y)

            end if
            write(fu,*) t(i), z(i), v(i)
        end do
                
           write(*,*) "	Procesamiento de datos exitoso lo datos se guardaron en >salida.dat<. abrelo!  :D"
    
    close(fu)
	!#####################################################################################
	! ajuste de la func. z(t) usando gnuplot
	
	print*,''
    print*,''
    print*,'2E.	Ajuste de la función z(t)= - g.alfa.t + g.alfa²(1-exp(-t/alfa) utilizando los puntos de datos.dat'
    print*,'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    print*,''
    print*,'	Los valores obtenidos fueron: alfa = 3.14392 ; g = 9.67978'

	
	
    !########################### CALCULO DEL TRABAJO        ##############################
    !--------------------------------------------------------------------------------
    !Realizamos la integral de la funcion F.vdt  entre t=0.0s y t = 16.0s
    print*,''
	print *, ''	
	print *, '2G.	Cálculo de W(t) (analítica) utilizando el método de integración de Simpson.'
	print *, '	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
	print *, ''		
    a = t(0)		!extremo izquierdo de integracion
    b = t(nlines-1) !extremo derecho de integracion
    n = 100 		!Numero par de division de subintervalos
    
    call simpson (a, b, n, Fv, int)
    
    
    print*, "	El trabajo calculado con Simpson es W(t) =", int
    
    
    !########################### CALCULO ALTERNATIVO DEL TRABAJO################################
    !--------------------------------------------------------------------------------
    print*,''
	print *, ''	
	print *, '2H.	Cálculo de W(z) utilizando el método de integración del Trapecio para puntos no equiespaciados.'	
	print *, '	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
	print *, ''    
	
    !DATOS OBTENIDOS DEL AJUSTE DE MINIMOS CUADRADOS
    alfa = 3.14392_wp
    m = 0.001_wp
    g = 9.67978_wp
    c = m/alfa
    allocate (fz(0: nlines-1))
    do i = 0 , nlines-1
        fz(i) = - m * g - c * v(i) 
    end do
    
    call trapecionoeq(z, fz, nlines , int )
    
    
    print*,'	El trabajo calculado con el met. del trapecio es W(z) =' ,int
    

    deallocate (fz)

    !#################### TRABAJO USANDO EL TEOREMA DE TRABAJO ENERGIA#########################
    !--------------------------------------------------------------------------------
    print*,''
	print*,''
	print*,'2I.	Calculamos W de forma analítica a partir de la energía cinética.'
	print*,'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
	print*, ''
	
     work = -1._wp * (1._wp/2._wp) * m *( (v(nlines-1))**2 - (v(0))**2 )
     
     print*,'	Trabajo con consideracion energeticas es =', work
     
    
    !#################### INTERPOLACION DE LA POSICION z(t) vs t USANDO LAGRANGE###############
    !--------------------------------------------------------------------------------
    print*,''
	print *, ''
	print*,'2J.	Interpolación de z(t) usando el método de pol. interpolado de Lagrange.'
	print*,'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
	print *, ''

    archivo_out = 'pol_lagrange.dat'
    c = 0.0_wp    
    n = 100
    
    open(newunit=fu, file=archivo_out)
    	write(fu,*) "   tiempo   ",  "    Posicion  "
    		do i = 0 , n
    	        c = t(0) + float(i) * ((t(nlines-1) - t(0))/float(n))
    	        call lagrange(nlines-1, t, z, c , pc)
    	        write(fu,*) c, pc
         end do
    close (fu)

    print *, '	Pol. de Lagrange: ver archivo >pol_lagrange.dat< y >figura2j.png'
	print *, ''
	     
    deallocate(t, z, v)
    
    !write(*,*) 'Los incisos 2A., 2C., 2D., y 2F. se encuentran tanto en el pdf de explicaciones\ncomo en la carpeta Gráficos.'
    !print *, 'Los incisos 2A., 2C., 2D., y 2F. se encuentran tanto en el pdf de explicaciones\ncomo en la carpeta Gráficos.'
    
    print *, 'Los incisos 2A., 2C., 2D., y 2F. se encuentran tanto en el pdf de explicaciones'
    print *, 'como en la carpeta Gráficos.'
    print *, ''    
    print *, ''
    print *, '( · u · )/'

end program parte2
