set term x11 persist

# parcial 2 - ejercicio 2

# MERCADO, Javier
# SOLLENDER, Jazmín

# parte II.C
# Gráfico de comparación entre métodos.

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Funcion posicion v(t) vs z(t) "
    set xlabel "Posicion"
    set ylabel "Velocidad"
    set grid
    #set logscale xy
    #set sample 500

##################################################
######      G R A F.   D A T O S            ######
##################################################

    # grafico error relativo biseccion    

          plot "salida.dat" u 2:3 title "Datos v vs z" w lp pointtype 7


##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura2d.png'
    replot

exit

