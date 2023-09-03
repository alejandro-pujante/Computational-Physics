*   1-71-app.f 
************************************************************************
* soluciones de la ecuacion tanh(a*k) - 2/x + 1
*
* empleo metodo de biseccion (subrutina) para calcular numericamente 
* los ceros de la ecuacion.
*
*     Alejandro Pujante Pérez                10.03.2021
************************************************************************

        include '../bisecc-rgm.f'     ! incluyo la subrutina biseccion  
        external f
        real f 
        common /well/a
        real :: a , a_min , a_max , paso
        integer :: i , n
        
* como en mi bucle necesito un paso(step) que no es real, necesito
* redefinir algunas variables
        
        a_min = 0       !intervalo donde quiero hallar mis solucioes..
        a_max = 5       !..y paso(step) que quiero para el bucle
        paso = 0.05
        
        n = nint( (a_max-a_min)/paso )  ! convierto el valor en entero

        nmax = 200        ! numero maximo de iteraciones     
        tol = 1.0e-5      ! tolerancia para hallar la solucion
        
* por comodidad para exportar los resultados, los escribire en
* un fichero .dat segun se vayan realizando.

        open(10 , file = '1-71-app.dat')
        
        write(10 , *)'nmax=' , nmax , 'tol=' , tol   
        write(10 , *)
        
        
        
        do i = 0 , n        ! inicio mi bucle ahora si con variables reales
        
            a = i*paso
        
            xl = 0.5     ! valores de xl y xr son constantes durante todo el bucle
            xr = 2.5
            
            call bisecc(f,xl,xr,nmax,tol,n,x)    ! llamo a la subrutina biseccion para hallar la soluciones.. 
                                                 !.. para cada valor de a
            e = - x**2 / 2.0
            
            write(*,*)'a =' , a , 'energia=' , e   ! muestro las soluciones
            
            write(10 , *)'a=' ,  a , 'E=' ,  e
        
        end do
        
        close(10)
        
        
        stop
        end program
        
************************************************************************
        function f(x)
************************************************************************        
        
        common /well/a           ! common para pasar los parametros a la funcion
        
        f = tanh(a*x) - 2.0/x + 1.0
        
        return
       
        end 
