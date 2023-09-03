*     0-07-app.f
************************************************************************
*  calculo del producto escalar y del producto vectorial de dos vectores
*  conocidas sus componentes
*
*  se usan variables dimensionadas
*           07.01.2014         Rafael Garcia Molina
************************************************************************

*voy a trabajar con dos vectores a y b
*el producto vectorial lo escribo como v

        dimension a(3) , b(3)       ! indico las dimensiones de cada uno 
        dimension v(3)
        
*leo las componentes de cada vector en filas

        open(10 , file = '0-07-app.in')   ! le digo que lea el .in
        
        read(10 , *) (a(i) , i = 1,3)   ! lee las componentes de cada vector
        read(10 , *) (b(i) , i = 1,3)    
        
        close(10)
        
*calculamos el producto escalar

        sum = 0.0     ! inicio el contador suma a 0
        
        do i = 1,3
            sum = sum+ a(i)*b(i)
        end do
        
        
*calculamos el producto vectorial

        v(1) = a(2)*b(3) - a(3)*b(2)    ! componentes del prod.vect
        v(2) = a(3)*b(1) - a(1)*b(3)    
        v(3) = a(1)*b(2) - a(2)*b(1) 
        
*escribo los resultados

        open(20 , file = '0-07-app.dat')                                  ! abro el archivo donde voy a escribir
        
        write(20 , *) 'componentes de los vectores originales'
        write(20 , *) 'primer vector a =' , (a(i) , i = 1,3)              ! escribo las componentes de cada vector y el resultado
        write(20 , *) 'segundo vector b =' , (b(i) , i = 1,3)
        write(20 , *)
        write(20 , 100) sum
        write(20 , 200) v(1) , v(2) , v(3)                            ! llamo a los formtaos 100 y 200 para poner 8 espacios y 4 de ellos decimales
        
        close(20)
        
        write(* , *) 'programa finalizado'
        
100     format(2x , 'el producto escalar a * b =' , f8.4)
200     format(2x , 'el producto vectorial a x b =' , f8.4)
        
        stop
        
        end
