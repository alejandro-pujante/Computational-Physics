*   4-76-ab-app.f 
************************************************************************
* solución al sistema de ecuaciones que modeliza una presa y 
* dos depredadores
* 
* utilizo el metodo de Runge-Kutta de orden 4
*
*     Alejandro Pujante Pérez                29.04.2021
************************************************************************



		include '../rk4.f'
        parameter (n=3)                      ! n = orden de la ecuacion diferencial (o num. de ecs. difs.)
        dimension y(n),dydx(n),yout(n)
        external derivs
        common/ene/nn
        
        real a , k , b1 , m1 , eps1 , b2 , m2 , eps2
        
        nn = n
        
* defino todas las constantes del problema

        a = 0.1
        k = 1.5
        b1 = 0.11
        m1 = 0.12
        eps1 = 1.1
        b2 = 0.25
        m2 = 0.17
        eps2 = 2.4
        
* valores del dominio donde voy a resolver la ecuacion diferencial

        x0 = 0.0
        xf = 900
        npasos = 1500
        
        h = (xf - x0) / real(npasos)
        
* condiciones iniciales del problema

        y(1) = 1.7
        
        y(2) = 1.0
        
        y(3) = 1.7
        
        
* abro fichero para guardar datos
       
       open(10 , file = '4-76-ab-app.dat')
       
       write(10,*)'x   y1   y2   y3 '
       write(10 , *) x , ( y(i), i=1 , n )
       
       
       do j = 1 , npasos
       
            call derivs(x , y , dydx)
            call rk4(y , dydx , n , x , h , yout , derivs)
            
            x = x0 + j*h
            
            do i = 1 , n
                y(i) = yout(i)
            end do
            
            write(10 , *) x , y(1) , y(2) , y(3) 
            
        end do
       
        
        close(10)
        
        write(*,*) 'programa finalizado con exito'
        

        stop
        
    
        end 
       

        
        
        subroutine derivs(x,y,dy)
        
        dimension y(nn) , dy(nn)
        common/ene/nn
        
        real a , k , b1 , m1 , eps1 , b2 , m2 , eps2
        
        a = 0.1
        k = 1.5
        b1 = 0.11
        m1 = 0.12
        eps1 = 1.1
        b2 = 0.25
        m2 = 0.17
        eps2 = 2.4
        
        
        
        dy(1) = a*y(1)*( 1-( y(1)/k) ) -( b1*y(2) + b2*y(3))*y(1)
        
        dy(2) = eps1*b1*y(1)*y(2) - m1*y(2)

        dy(3) = eps2*b2*y(1)*y(3) - m2*y(3)
        
        return
        
        end 
        
        
