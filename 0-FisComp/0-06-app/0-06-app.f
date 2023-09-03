*     0-06-app.f
************************************************************************
*  calculo del vector unitario dado las componentes de un vector
*  como dato de entrada
*  
*           23.02.2021        Alejandro Pujante Pérez
************************************************************************

* el vector original de entrada será el vector a

        dimension a(3) , v(3)   !La dimension del vector a será de 3 
        
* lectura de las componentes del vector
        
        open(10 , file = '0-06-app.in')   
        
        read(10 , *) (a(i) , i = 1,3)
        
        close(10)
        
* calculo del módulo al cuadrado del vector

        amod2 = 0.0
        
        do i = 1,3
            amod2 = amod2 + a(i)**2
        end do

* calculo el modulo del vector

        amod = sqrt(amod2)
        
* las componentes del vector unitario son

        v(1) = a(1) / amod
        v(2) = a(2) / amod
        v(3) = a(3) / amod
        
   
* escritura de los resultados

        open(20 , file = '0-06-app.dat')

        write(20 , 200) (a(i) , i = 1,3)
        write(20 , 100) v(1) , v(2) , v(3)
        
        close(20)
        
        write(* , *) 'programa finalizado con exito'
        

100     format(2x , 'el vector unitario al vector dado es' , f8.4)
200     format(2x , 'el vector introucido es' , f8.4)

        
        stop
        
        end
