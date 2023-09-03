*    1-02SustRep-app.f
************************************************************************
*  calculo de los ceros de la funcion
*      f(x)=0.5*exp(x/2) - x
*  mediante el metodo de sustitucion repetida
*
* (no defino la funcion mediante "function")
*
*     Alejandro Pujante Pérez       28.02.2021
************************************************************************


        x0 = 0.5      ! valor inicial para aplicar sustitucion rep.
        tol = 1.0e-3  ! tolerancia con el que se pretende obtener el resultado
        
        kmax = 20     ! numero maximo de iteraciones
        
* abro fichero para guardar secuencia de valores obtenidos

        open(10 , file = '1-02-SustRep-Izq-app.dat')  ! la izquierda del cero
        
        write(10 , *) '1-02-SustRep-Izq-app.dat'
        
        open( 10 , file = '1-02-SustRep-Der-app.dat' )  ! la derecha del 0
        
        write(10 , *) '1-02-SustRep-Der-app.dat'
        
        
        write(10,*) 'x0=',x0,'    tol=',tol   ! escribo en 10 datos de tolerancia y x0
        write(*,*) 'x0=',x0,'    tol=',tol
        
        
        do k=1,kmax
            x = 0.5*exp(x0/2.0)
            write(* , *) k , x     ! le digo que me vaya escribiendo las iteraciones en pantalla y en 10
            write(10 , *) k , x 
            
            dif = abs(x0-x)        !criterio de error absoluto
            
            if (dif.gt.tol) then
                x0 = x
                
            else
            
            write(* , *) 'numero de iteraciones =' , k , 'x=' , x
            write(* , *) 'se alcanzo la precision deseada'
            write(* , *)
            write(* , *) 'programa finalizado'
            write(10 ,*) 'se alcanzo la precision deseada'
            
            stop
            
            end if
            
        end do
        
        write(*,*) 'num. iter.=', kmax,'   x=',x
        write(*,*) 'no se alcanzo la precision deseada'
        write(10,*) 'num. iter.=', kmax,'   x=',x
        write(10,*) 'no se alcanzo la precision deseada'
        close(10)
      
        write(*,*)
        write(*,*) 'programa finalizado'

        stop
        end
        
        
        
            
            
            
