module funciones
use mod_prec
implicit none
contains
    real(wp) function pos(x)
        real(wp), intent(in)        :: x
        pos = -9.67978_wp*3.14392_wp*x + 9.67978_wp*3.14392_wp**2._wp*(1._wp-exp(-x/3.14392_wp)) 
    end function pos
    
   real(wp) function vel(x)
        real(wp), intent(in)        :: x
        vel =  9.67978_wp*3.14392_wp*(exp(-x/3.14392_wp)-1._wp)
    end function vel

     real(wp) function F(x)
        real(wp), intent(in)        :: x
        F =  0.001_wp*9.67978_wp*exp(-x/3.14392_wp)
    end function F

     real(wp) function Fv(x)
        real(wp), intent(in)        :: x
        Fv =  0.001_wp*(9.67978_wp**2)*3.14392_wp*(exp(-2._wp*x/3.14392_wp) - exp((-x/3.14392_wp)))
    end function Fv
    
    !cuando terminamos de escribir todo se nos ocurrió que podríamos haber definido unas variables
    !alfa y  g con los valores en vez de escribirlo cada vez. :(


end module funciones
