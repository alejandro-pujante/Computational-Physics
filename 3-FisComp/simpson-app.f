*   simpson-app.f 
************************************************************************
* subroutina para calcular la integral de una función entre dos
* valores (final e inicial) por el método de simpson.
* 
* 
*     Alejandro Pujante Pérez                22.03.2021
************************************************************************

       subroutine simpson(f,xin,xsub,n,suma)  
       real f
       external f


       h = (xsub - xin) / real(n)    ! defino el paso
       
       
       sodd = 0.0      ! inicializo la suma impar a 0.0
       
       seven = 0.0     ! inicializo la suma par a 0.0
       
       do i = 1 , n-1 , 2       ! bucle para calcular los terminos impares
       
            x = xin + i*h
            sodd = sodd + f(x)
       
       end do
       
       
       
       do i = 2 , n-2 , 2       ! bucle para calcular los terminos pares
            
            x = xin + i*h
            seven = seven + f(x)
            
       end do
       
       
       suma = h/3.0*( f (xin) + 4.0*sodd + 2.0*seven + f(xsub) )   ! resultado de la integral
       

       return
       end
