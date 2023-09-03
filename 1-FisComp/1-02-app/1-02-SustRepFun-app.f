*    1-02-SustRepFun-app.f
************************************************************************
*  calculo de los ceros de la funcion
*      f(x)=0.5*exp(x/2) - x
*  mediante el metodo de sustitucion repetida
*
* (defino la funcion mediante "function")
*
*     Alejandro Pujante Pérez      28.02.2020
************************************************************************

        external f
        real f
        
        x0 = 1.0
        tol = 1.0e-6
        
        kmax = 20
        
*abro fichero para guardar secuencia de valores

        open(10 , file = '1-02-SustRepFun-Izq-app.dat') 
        write(10,*) '1-02-SustRepFun-Izq-app.dat'
        
        open(10 , file = '1-02-SustRepFun-Der-app.dat')
        write(10 ,*) '1-02-SustRepFun-Der-app.dat'
        
        write(10 ,*) 'x0 =' , x0 , 'tol =' , tol
        write(* ,*) 'x0 =' , x0 , 'tol =' , tol
        
        do k=1 , kmax
            x = 0.5*exp(x0/2.0)
            
            x = f(x)
            
            write(* , *) k , x
            write(10 , *) k , x
            
        dif = abs(x0-x)
        
            if (dif.gt.tol) then
            x0 = x
            
            else
            write(*,*) 'num.iter.=',k,'   x=',x
            write(*,*) 'se alcanzo la precision deseada'
            write(*,*)
            write(*,*) 'programa finalizado'
            write(10,*) 'num.iter.=',k,'   x=',x
            write(10,*) 'se alcanzo la precision deseada'
            close(10)
        
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
        
        
        
        function f(x)
        
        f = 0.5*exp(x/2)
        return
        end
        
        
