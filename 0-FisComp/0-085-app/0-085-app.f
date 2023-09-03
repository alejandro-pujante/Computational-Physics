*      0-85-app.f
************************************************************************
* programa para realizar la suma
*
* s = sum_{n=1}^\infty (3n - 2)/( 2^(n-1) )
*
* y calcular la iteración n para el cual tenemos un error del 1%
*
*      Alejandro Pujante Pérez                       24.02.2021
************************************************************************


        nmax = 200       ! numero máximo de iteraciones
        tol = 1.0e-2     ! error con el que admito la precision
        s0 = 0.0         ! inicio la variable donde voy almacenando la suma a 0.0
        
        write(* , *) 'tol=' , tol
        
        do n = 1 , nmax                                  ! inicio el sumatorio desde 1 a nmax
            s = s0 + (3*real(n) - 2)/(2**(real(n)-1))    ! defino el error relativo
            dif = abs( (s-s0) / (0.5*( s0+s)) )          
            
            if (dif.le.tol) then                                     
                write(* , *) 'se ha alcanzado la convergencia'
                write(* , *) 'iteracion' , n , 'la suma es s=' , s
                
                goto 100
                
            else
                continue
                
            end if
            
            s0 = s      ! redefino la suma de nuevo
            
        end do
        
        
        write(* , *) 'no se ha alcanzado la convergencia'
        write(* , *) 'iteracion=' , nmax , 's=' , s
100     write(* , *)
        write(* , *) 'programa finalizado'
        
        
        
        
        stop
        end
