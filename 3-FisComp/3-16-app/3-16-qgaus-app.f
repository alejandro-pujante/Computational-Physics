*     3-16-qgaus.f
***********************************************************************
* solucion de la itegral definida de exp(x) de 0 a 1
*
* utilizo subrutina qgaus
* 
*     Alejandro Pujante Pérez                22.03.2021
************************************************************************

        include '../qgaus.f'
        external f
        real f
        
        xin= 0.0           ! punto inicial
        xsub = 1.0         ! punto final
        
        
        exct = exp(1.0) - 1   ! resultado analitico de la integral
        
        call qgaus(f,xin,xsub,suma)  ! llamo a la subrtutina 
        
        error = exct - suma
        
        write(*,*) 'el valor de la integral es' , suma
        write(*,*)
        write(*,*)'el error cometido es',error 
        
        stop
        end
        
***********************************************************************
        function f(x)    ! defino la funcion del integrando
        
        f = exp(x)
        
        return
***********************************************************************
        end
