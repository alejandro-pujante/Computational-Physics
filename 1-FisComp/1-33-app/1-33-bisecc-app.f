*     1-33-bisecc-app.f
************************************************************************
*  calculo de los ceros de la funcion f(x) = tan(x) + cosh(x)
*  mediante el metodo de biseccion.
*
*     Alejandro Pujante Pérez               10 .03.2021
************************************************************************

* valores iniciales por la derecha y la izquierda del cero buscado
           
            xl = 1         ! valor por la izquierda
            xr = 2         ! valor por la derecha
            
            tol = 1.0e-5      ! tolerancia para obtener el resultado
            
            kmax = 200        ! numero maximo de iteraciones
            
            fl = f(xl)         ! defino fl como el valor de f en el punto xl
            fr = f(xr)
            
* compruebo si los valores iniciales son correctos (si fl y fr tienen el mismo signo)

            if(fl*fr.gt.0.0) goto 100
            do k = 1, kmax
                xm = (xr+xl) / 2.0         ! xm es el valor de x entre xl y xr
                fm = f(xm)
            
                write(* , *) k , xm , dif     ! escribo en pantalla la evolucion
            
                if (fm*fr .le. 0.0) then
                    xl = xm
                    fl = fm
                
                else
                    xr = xm
                    fr = fm
                
                end if
            
            
                dif = abs(xr - xl)
            
                if (dif .le. tol) goto 200   ! si alcanza el valor min de la tolerancia sale del bucle
            
            end do
        
        
        write(*,*) 'num. iter.=',kmax,'   x=',xm
        write(*,*)
        write(*,*) 'no se alcanzo la precision deseada'
        stop
200     write(*,*) 'num. iter.=',k,'      x=',xm
        write(*,*)
        write(*,*) 'se alcanzo la precision deseada'
        stop
100     write(*,*) 'datos iniciales mal elegidos'
        stop
        end    
            
        
        function f(x)     ! defino la funcion cuyos ceros quiero calcular

        f= sin(x) + cos(x)*cosh(x)
       
        return
      
        end 
