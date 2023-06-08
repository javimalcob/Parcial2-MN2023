set term x11 persist

# parcial 2 - ejercicio 2

# MERCADO, Javier
# SOLLENDER, Jazmín

# parte II.C
# Gráfico de comparación entre métodos.

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Polinomio Lagrange z(t) vs t "
    set xlabel "Tiempo"
    set ylabel "Polinomio Lagrange z"
    set grid
    #set logscale xy
    #set sample 500
 
    # Posicion respecto del tiempo(x) obtenida mediante el ajuste
    z(x) = -9.67978*3.14392*x + 9.67978*3.14392**2*(1-exp(-x/3.14392))
##################################################
######      G R A F.   D A T O S            ######
##################################################
    
    # grafico error relativo biseccion    
    
          plot "pol_lagrange.dat" u 1:2 title "Polinomio lagrange z vs t" w lp pointtype 7  
          replot "datos.dat" u 1:2 title "puntos medidos z vs t" w lp pointtype 7
          replot z(x)
##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura2j.png'
    replot

exit
