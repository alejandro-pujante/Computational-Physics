*     0-02-app.f
************************************************************************
*  programa para escribir algunas operaciones básicas entre números
*
*  las operaciones se realizan cuando se escribe el resultado
*
*     Alejandro Pujante Pérez                       21.02.2021
************************************************************************
* Estas operaciones son entre números reales, poner los .0

        pi=acos(-1.0) !Defino el valor de pi con la funcio arcocoseno
        
        write(*,*)'Los resultados a las operaciones son los siguientes:'
        
        write(*,*) '(a)' , 4.18 + 31.0
        write(*,*) '(b)' , 12.0 / 5.0
        write(*,*) '(c)' , 9.0 / 17.0
        write(*,*) '(d)' , (2.5)**3 - 10.17
        write(*,*) '(e)' , 4.0 + 5.0/7.0
        write(*,*) '(f)' , 23.5e2 - 10.0*pi
        
        write(*,*) 'El programa ha finalizado'
        
        stop
        end
      

