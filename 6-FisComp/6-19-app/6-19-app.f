*   6-19-app.f
************************************************************************
*  Distribucion de temperaturas en estado estacionario de una placa 
*  circular con condiciones de contorno:
*   
*   100 grados en el primer y tercer cuadrante 
*   resto de los cuadrantes 0 grados
*
*   Alejandro Pujante Perez                   13.05.2021
************************************************************************

        parameter (nx=500,ny=500)
        parameter (itermax=1000)
        dimension t(0:nx,0:ny)
        
        pi=acos(-1.0)
        
        
************************************************************************
*     DATOS FISICOS DEL PROBLEMA
************************************************************************
* Rango en la variable x
        x0= -1.0
        xf= 1.0
* Rango en la variable y
        y0= -1.0
        yf= 1.0
        
* Condiciones de contorno (temperatura):
        t1= 100.0     ! temperatura en el primer cuadrante
        t2= 0.0       ! temperatura en el segundo cuadrante
        t3= 100.0     ! temperatura en el tercer cuadrante
        t4= 0.0       ! temperatura en el cuatro cuadrante
***********************************************************************
      
        hx=(xf-x0)/real(nx)   ! paso en x
        hy=(yf-y0)/real(ny)   ! paso en y
        h = hx
      
* el parametro w optimo para aplicar SOR

        w= 4.0/(2.0+sqrt(4.0-(cos(pi/nx)+cos(pi/ny))**2))
        

* En cada punto interior propongo valores de la temperatura 

        do i=1,nx-1
            do j=1,ny-1
            y=y0+j*hy
            x=x0+i*hx 
            r=sqrt(x**2 + y**2)
            if (r.le.1.0+h) then
              t(i,j)= 25.0  
            end if
           end do
        end do
        
        
        
* Recorro los cuadrantes (segun x,y) para escribir las condiciones de contorno:

        do i=0,nx
           do j=1,ny-1
           y = y0 + j*hy
           x= x0+i*hx  
            
           r = sqrt(x**2 + y**2)
          if (r.gt.1.0-h.and.r.lt.1.0+2.0*h
     &    .and.x.ge.0.0 .and.y.ge.0.0) then
            t(i,j)= t1
          else
            continue
          end if
            
          if (r.gt.1.0-h.and.r.le.1.0+2.0*h
     &    .and.x.lt.0.0 .and.y.ge.0.0) then
            t(i,j)= t2
          else
            continue
          end if
                
          if (r.gt.1.0-h.and.r.le.1.0+2.0*h
     &    .and.x.lt.0.0 .and.y.lt.0.0) then
            t(i,j)= t3
          else
            continue
          end if
           
            
          if (r.gt.1.0-h.and.r.le.1.0+2.0*h
     &    .and.x.ge.0.0 .and.y.lt.0.0) then
            t(i,j)= t4
          else
            continue
          end if

            
            
           end do
            
        end do
        
 
        close(10)
        



* Se resuelve el sistema de ecuaciones por Gauss-Seidel

        do iter=1,itermax               
            do i=1,nx-1
                do j=1,ny-1
                y=y0+j*hy
                x=x0+i*hx 
                r=sqrt(x**2 + y**2)
                if (r.le.1.0-h) then
                    told=t(i,j)
                    t(i,j)=0.25*(t(i+1,j)+t(i-1,j)+t(i,j+1)+t(i,j-1))   ! no hay termino fuente
                    t(i,j)=w*t(i,j)+(1.0-w)*told
                end if
                end do
            end do
        end do
      
      
      
* Escribo los resultados

       open(10,file='6-19-app.dat',status='unknown')
      
        do i=0,nx
            x=x0+i*hx
            do j=0,ny
                y=y0+j*hy
                r = sqrt(x**2+y**2)        ! solo resuelvo para r<1
                if (r.le.1.0-h) then
                    write(10,*) x,y,t(i,j)
                end if
            end do
            write(10,*)
        end do
      

      
        write(*,*) 'programa finalizado'


        stop
     
        end

    
