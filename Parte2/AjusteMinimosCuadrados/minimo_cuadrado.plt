set term x11 persist
     
     
     
     f(t) =  -g*a * t + g*a**2 *(1 - exp(-t/a))   # Aca defino el modelo de la funcion para ajustar

    # USO LA FUNCION FIT Y EL ARCHIVO DE DATOS usando m y q como los parametros a determinar
    fit f(x) 'datos.dat' using 1:2 via g, a    
	
exit
