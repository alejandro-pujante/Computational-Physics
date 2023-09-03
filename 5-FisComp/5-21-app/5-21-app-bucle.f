*     5-21-app-bucle-app.f
************************************************************************
* solución a ecuación diferencial con condiciones de contorno
*  
*  y(1)=2  ;   y(3)=-1   mediante el método del disparo con 
* un bucle para proponer derivadas iniciales
* 
*     Alejandro Pujante Pérez             4.05.2021
************************************************************************

        include '../rk4.f'
        parameter (n=2)
        dimension y(n),dydx(n),yout(n)
        external derivs
        common/ene/nn 
       
        nn = n
        x0 = 1.0
        xf = 3.0
        
        yin = 2.0
        yfin = -1.0
        
        yp0 = -1.0
        yp1 = -3.0
        
        kmax = 20
        error = 10e-4
        
        m = 600
        h = (xf-x0) / real(m)
        
        x = x0
        y(1)= yin
        y(2)= yp0
        
        do i= 1,m
            call derivs(x,y,dydx)
            call rk4(y,dydx,n,x,h,yout,derivs)
            x = x0 + i*h
            
            do j=1 , n
                y(j)=yout(j)
            end do
        end do
        
        y0 = y(1)

        x = x0
        y(1)=yin
        y(2)=yp1
        
        do i=1, m
            call derivs(x,y,dydx)
            call rk4(y,dydx,n,x,h,yout,derivs)
            x = x0+i*h
            do j=1,n
                y(j)=yout(j)
            end do
        end do
        
        y1 = y(1)
        
        
        do k=1 , kmax
            
            x = x0
            y(1)=yin
            yp2= yp1 - (y1-yfin)*(yp1-yp0) / (y1-y0)
            y(2)= yp2
            
            
        do i = 1 , m
            call derivs(x,y,dydx)
            call rk4(y,dydx,n,x,h,yout,derivs)
            x = x0 + i*h
            do j = 1,n
                y(j)=yout(j)
            end do
        end do
        
        dif = abs(y(1) - yfin)
        if (dif.le.error) goto 10
        
        
        y0 = y1
        y1 = y(1)
        yp0 = yp1
        yp1 = yp2
        
       end do
            

10     write(*,*)'En la iteracion' , k , 'se obtiene una yp de'
       write(*,*)yp2
       
       
       open(10 , file = '5-21-bucle-app.dat')
       x = x0
       y(1)= yin
       y(2)= yp2
       
        do i=1 , m
            call derivs(x , y , dydx)
            call rk4(y , dydx,n,x,h,yout,derivs)
                
            x = x0 + i*h
            do j=1 , n
                y(j)=yout(j)
            end do
            write(10,*)x , y(1) , y(2)
                
        end do
       
       close(10)
       
       write(*,*)'programa finalizado'
       
       
       stop
       end
       
       
       
        subroutine derivs(x , y , dy)
        
        dimension y(nn),dy(nn)
        common/ene/nn
        
        dy(1)=y(2)
        
        dy(2) = x + ( 1.0 - x/5.0 )*y(2)*y(1)
        
        return
        end
       
