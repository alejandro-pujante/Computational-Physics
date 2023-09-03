*   3-10-app.f 
************************************************************************
* obtener la capacidad termica molar de un aislante a partir
* de su ecuacion integral.
*  
*     Alejandro Pujante Pérez                24.03.2021
************************************************************************

        include '../simpson-app.f'
        external f
        real f

        n = 100                     ! numero de iteraciones
        
        tmin = 5.0
        tmax = 1290.0
        nt = 500
       
        h = (tmax - tmin) / nt
        
        open(10 , file = '3-10-app.dat')
        
        do i = 1 , nt
        
            T = tmin + i*h
            xin= 0.01
            xsub = 576 / T
            
            call simpson(f,xin,xsub,n,suma)
            
            total = (T/645)**3*suma
            
            write(10 , *) total , ','
           
            write(*,*) T , total

        end do
        
        close(10)
        
        stop
        end
        
***********************************************************************
        function f(x)    ! defino la funcion del integrando
        
        f = ( (x**4*exp(x)) / (exp(x)-1)**2  )
        
        return
***********************************************************************
        end

