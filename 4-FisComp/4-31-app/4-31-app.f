*   4-31-app.f 
************************************************************************
* solución al sistema de ecuaciones que modeliza un pendulo elastico
* 
* utilizo el metodo de Runge-Kutta de orden 4
*
*     Alejandro Pujante Pérez                29.04.2021
************************************************************************

		

        include '../rk4.f'
        parameter (n=4)             ! n = orden de la ecuacion diferencial (o num. de ecs. difs.)
        dimension y(n),dydx(n),yout(n)
        external derivs
        common/ene/nn
        real m
        real k
        real lambda
        real L
        nn = n
        
* defino todas las constantes del problema        
        
        L = 1.0
        k = 3.1
        g = 9.81
        m = 0.25
        wr = sqrt( k / m )
        wp = sqrt(g / L)
        lambda = (wr**2) / (wp)**2
        
* valores del dominio donde voy a resolver la ecuacion diferencial
        
        x0 = 0.0
        xf = 15.0
        npasos = 1500
        
        h = (xf - x0) / real(npasos)

* condiciones iniciales del problema

        y(1) = L
        y(3) = 0.0           
        
        y(2) = 0.0
        y(4) = 0.0
        
        
* abro fichero .dat y empiezo a resolver la ecuacion diferencial

        open(10 , file = '4-31-app.dat')
        
        x = x0
        
        write(10,*)'x   y1   y2   y3   y4'
        write(10 , *) x , ( y(i), i=1 , n )

        
        do j=1 , npasos
            
            call derivs(x , y ,dydx)
            call rk4(y , dydx , n , x , h , yout , derivs)
        
            x = x0 + j*h
            
            do i = 1 , n
                y(i)= yout(i)
            end do
        
            write(10 , *) x , y(1) , y(2) , y(3) , y(4)
            
        end do
        
        close(10)
        
        write(*,*) 'programa finalizado con exito'
        
        stop
        
        end
        
        
        

        
        subroutine derivs(x,y,dy)
        
        dimension y(nn) , dy(nn)
        common/ene/nn
        
        real m
        real k
        real lambda
        real L
        
        L = 1.0
        k = 3.1
        g = 9.81
        m = 0.25
        wr = sqrt( k / m )
        wp = sqrt(g / L)
        lambda = (wr**2) / (wp)**2
        
        dy(1) = y(2)
        dy(2) = -wr**2*y(1) + g*lambda*y(1)/(sqrt(y(1)**2 + y(3)**2))
        dy(3) = y(4)
        dy(4) = -g - wr**2*y(3) + g*lambda*y(3)/(sqrt(y(1)**2+y(3)**2))
        
        
        
        
        return
        end
        
        
        
        
         
        
        
        
