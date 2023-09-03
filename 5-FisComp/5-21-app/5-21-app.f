*     5-21-app-app.f
************************************************************************
* solución a ecuación diferencial con condiciones de contorno
*  
*  y(1)=2  ;   y(3)=-1   mediante el método del disparo con 7 tiros
* 
*     Alejandro Pujante Pérez             4.05.2021
************************************************************************

        include '../rk4.f'
        parameter (n=2)
        dimension y(n),dydx(n),yout(n)
        external derivs
        common/ene/nn 
       
        nn=n
        
* valores inicial (x0) y final (xf) de la variable independiente
     
        x0=1.0
        xf=3.0
        
        
* condiciones de contorno para la funcion
     
        y0=2.0     ! valor de la funcion en el origen del dominio
        yf=-1.0    ! valor de la funcion en el final del dominio
        
        npasos = 400
        
        h=(xf-x0)/real(npasos)     ! paso con que se recorre "x"
        
        yp1 =-1.0
        yp2 =-3.0
        
       
                
        do k = 1 , 7
        
            x = x0 
            
            y(1) = y0 
            
            if(k.eq.1) then
                y(2)=yp1
                open(10 , file = '5-21-1-app.dat')
                write(10,*)'    x              y              y-prima'
                
            else if(k.eq.2) then
                y(2) = yp2
                open(10 , file = '5-21-2-app.dat')
                write(10,*)'    x              y              y-prima'
           
            else if(k.eq.3) then
                y(2)=yp2-(yf2-yf)*(yp2-yp1)/(yf2-yf1)
                yp3 =y(2)
                open(10 , file = '5-21-3-app.dat')
                write(10,*)'    x              y              y-prima'
                
            else if(k.eq.4) then
                y(2)= yp3-(yf3-yf)*(yp3-yp2)/(yf3-yf2)
                yp4 = y(2)
                open(10 , file = '5-21-4-app.dat')
                write(10,*)'    x              y              y-prima'
           
            else if(k.eq.5) then
                y(2)= yp4-(yf4-yf)*(yp4-yp3)/(yf4-yf3)
                yp5 = y(2)
                open(10 , file = '5-21-5-app.dat')
                write(10,*)'    x              y              y-prima'
                
            else if(k.eq.6) then
                y(2)= yp5-(yf5-yf)*(yp5-yp4)/(yf5-yf4)
                yp6 = y(2)
                open(10 , file = '5-21-6-app.dat')
                write(10,*)'    x              y              y-prima'
            
            else if(k.eq.7) then
                y(2)= yp6-(yf6-yf)*(yp6-yp5)/(yf6-yf5)
                open(10 , file = '5-21-7-app.dat')
                write(10,*)'    x              y              y-prima'
        
            end if
            
            write(10,*) x,y(1),y(2)           ! escribo resultados al principio de cada intento
            
            do i=1 , npasos
            
                call derivs(x , y , dydx)
                call rk4(y , dydx,n,x,h,yout,derivs)
                
                x = x0 + i*h
                do j=1 , n
                    y(j)=yout(j)
                end do
                write(10,*)x , y(1) , y(2)
                
            end do
                
            close(10)
            
            if(k.eq.1) yf1=y(1)  ! guardo resultado al final del intento 
            if(k.eq.2) yf2=y(1)  
            if(k.eq.3) yf3=y(1)  
            if(k.eq.4) yf4=y(1) 
            if(k.eq.5) yf5=y(1)
            if(k.eq.6) yf6=y(1)
            
        end do
        
            
            
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
        

