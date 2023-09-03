*   3-52-app.f 
************************************************************************
* calculo del potencial producido por un cuadrante de circulo
* con una densidad de carga constante.
*  
*     Alejandro Pujante Pérez                29.03.2021
************************************************************************

        
        include '../simpson-app.f'      ! incluyo la subrutina simpson
        external h
        common/npuntosy/ny         ! common para pasarle los parámetros a la funcion
        common/zeta/z
        
        xinf = -0.6366       ! limites de integracion de la integral en x
        xsup = 0.8634
       
        nx = 1000
        ny = nx
         
        zinicial = 0.1       ! valores de z inicial y final para el bucle
        zfinal = 20.0
        npasos = 500
        
        dencarga = 2.8294    ! densidad de carga de la superficie
        
        
        open(10 , file = '3-52-app.dat' )   ! abro un archivo .dat para guardar los valores del bucle
        
        write(10,*)'z   v    vq      dif' 
        
        o = (zfinal - zinicial) / npasos      ! defino la distancia del paso para el bucle 
            
        tol = 1.0     ! tolerancia del 1% entre los dos valores del potencial
       
        do i = 1 , npasos     ! empiezo el bucle para z
    
            z = zinicial + i*o    
        
            call simpson(h,xinf,xsup,nx,sx)   ! resuelvo la integral para varios valores de z
            
            v = dencarga*sx         ! el potencial final sera el resultado de la integral por la densidad de carga
            
            vq = 5.0 / z            ! el potencial creado por la carga puntual es carga entre distancia
            
            dif = ( abs(vq - v) / vq)*100    ! criterio de error relativo entre los dos potenciales
                                             
*            if (dif.le.tol) then           ! si la diferencia entre los dos valores es menor que el 1%..
*                write(*,*)z                ! ..escribe el valor de z donde ha ocurrido
*                stop                         
*            else                           ! IMPORTANTE : Este apartado de codigo esta comentado para que no interrumpa el codigo del apartado a..
*                continue                   ! si se quiere comprobar, quite los asteriscos del margen izquierdo y compile el programa
*            end if
            
            write(10,*)z , v , vq , dif
            
        end do
        
        
        stop
        end
        
************************************************************************
       
        function h(x)
        external f
        common /npuntosy/ny
        common /equis/xx
        
        xx = x
        
        yinf = -0.6366                                  ! limites inferior y superior de y, que son funciones
        ysup = sqrt(1.5**2 - (x+0.6366)**2 ) -0.6366
        
        call simpson(f,yinf,ysup,ny,sy)                 ! resuelvo la integral h
        h = sy
       
        return
        end
        
************************************************************************

        function f(y)
        common/equis/xx      ! le paso la variable x con un common para no llamarla igual
        common/zeta/z        ! anado el parámetro z aunque no vaya a integrar en el
        
        f = 1.0 / sqrt(xx**2+y**2+z**2)    ! funcion a integrar

        return
        end        
    
