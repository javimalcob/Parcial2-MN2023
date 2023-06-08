set term x11 persist

# parcial 2 - ejercicio 2

# MERCADO, Javier
# SOLLENDER, Jazmín

# parte II.C
# Gráfico de comparación entre métodos.

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Funcion posicion(t) (ajust.)"
    set xlabel "tiempo"
    set ylabel "posicion"
    set grid
    #set logscale xy
    set sample 500

    # Posicion respecto del tiempo(x) obtenida mediante el ajuste
    z(x) = -9.67978*3.14392*x + 9.67978*3.14392**2*(1-exp(-x/3.14392))      
    
    #Velocidad respecto del tiempo(x) obtenida por derivar la z(x)
    #v(x) = 9.68*3.14*(exp(-x/3.14)-1)
    
##################################################
######      G R A F.   D A T O S            ######
##################################################

    # grafico error relativo biseccion    

    plot "salida.dat" u 1:2 title "Datos z vs t" w p pointtype 7
    replot z(x)

##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura2f.png'
    replot

exit
