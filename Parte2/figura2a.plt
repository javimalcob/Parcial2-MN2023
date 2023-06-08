set term x11 persist

# parcial 1 - ejercicio 2

# MERCADO, Javier
# SOLLENDER, Jazmín

# parte II.A
# Gráfico de comparación entre métodos.

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Funcion posicion z(t) vs t "
    set xlabel "Tiempo"
    set ylabel "Posición"
    set grid
    #set logscale xy
    #set sample 500

##################################################
######      G R A F.   D A T O S            ######
##################################################
    
    # grafico error relativo biseccion    
    
          plot "datos.dat" u 1:2 title "Datos z vs t" w lp pointtype 7  
    
      
##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura2a.png'
    replot

exit
