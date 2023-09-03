*   3-16-app.f 
************************************************************************
* solucion de la itegral defoinida de exp(x) de 0 a 1
*
* utilizo subrutina simpson para el calculo  
* 
*     Alejandro Pujante Pérez                22.03.2021
************************************************************************

        include '../simpson-app.f'
        external f
        real f
        
        xin= 0.0           ! punto inicial
        xsub = 1.0         ! punto final
        
        n = 100            ! numero de iteraciones
        
        exct = exp(1.0) - 1   ! resultado analitico de la integral
        
        call simpson(f,xin,xsub,n,suma)  ! llamo a la subrtutina 
        
        error = exct - suma
        
        write(*,*) 'el valor de la integral es' , suma
        write(*,*)
        write(*,*)'el error cometido es',error,'con',n,'iteraciones'
        
        stop
        end
        
***********************************************************************
        function f(x)    ! defino la funcion del integrando
        
        f = exp(x)
        
        return
***********************************************************************
        end
