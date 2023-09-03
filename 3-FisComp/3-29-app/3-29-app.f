*   3-29-app.f 
************************************************************************
* calculo de la intensidad en difraccion en funcion de la 
* distancia a la que nos alejamos
*  
*     Alejandro Pujante Pérez                28.03.2021
************************************************************************

        include '../simpson-app.f'
        external fc , fs
        real fc , fs
        
        
        n = 1000
        
        vmin = -5.0        ! valores de v minimos y maximos
        vmax = 5.0
        npasos = 1500      ! defino los pasos para el bucle
        
        vinf = 0.0
        
        h = (vmax - vmin) / (npasos)      ! distancia del paso para el bucle
        
        open(10 , file = '3-29-app.dat')    
        
        write(10,*)'  v   i    sumac    sumas'
        
        do i = 1 , npasos        ! empiezo el bucle para v
            
            vsup = vmin + i*h
            
            call simpson(fc,vinf,vsup,n,sumac)    ! hago la integral para la funcion con coseno y seno
            
            call simpson(fs,vinf,vsup,n,sumas)
            
            re = 0.5*( (sumac + 0.5)**2 + (sumas + 0.5)**2  )        ! expresion para la intensidad
            
            write(10,*)vsup , re , sumac , sumas
            
        end do
        
        close(10)
            
        
        stop
        end
        
************************************************************************
        function fc(t)       ! defino la funcion de la integral del coseno
        
        pi = acos(-1.0)      ! defino el numero pi
        
        fc = cos( (pi*t**2) / 2 )
        
        return
        end
************************************************************************
        function fs(t)       ! defino la funcion de la integral del seno
        
        pi = acos(-1.0)
        
        fs = sin( (pi*t**2) / 2 )
        
        return
        end
************************************************************************
    
    
