*   2-67-app.f 
************************************************************************
* soluciones al sistema de ecuaciones 
*
*
*     Alejandro Pujante Pérez                20.03.2021
************************************************************************

        include '../bisecc-rgm.f'     ! incluyo la subrutina biseccion  
        external f
        real f 
        
       
        nmax = 200           
        tol = 1.0e-7      
        
        xl1 = -0.7   
        xr1 = -0.5
        
        xl2 = 1.6
        xr2 = 1.67
 
        write(* , *)'tol =',  tol
        write(*,*)
 

    
      
        call bisecc(f,xl1,xr1,nmax,tol,n,x1) 
         
        
        write(*,*)'la solucion por la izquierda es    x=' , x1
        
        call bisecc(f,xl2,xr2,nmax,tol,n,x2) 
        
        write(*,*)'la solucion por la derecha es      x=' , x2
                                                    
        
        stop
        end program
        
************************************************************************
        function f(x)
************************************************************************        
        
                
        
        f = acos( (1.9-1*cos(x)) / 2 ) - asin( (1.1-1*sin(x) ) / 2  )
        
        return
       
        end 
