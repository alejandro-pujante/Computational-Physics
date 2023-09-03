*     2-51-SolSist-app.f
************************************************************************
*   solucion del sistema de ecuaciones de 5 ecuaciones
*   con 5 incógnitas
*   por el metodo de descomposicion LU
*
*     Alejandro Pujante Pérez          17.03.2021
************************************************************************
      include '../ludcmp.f'
      include '../lubksb.f'
      parameter (n=5,np=5)     ! phys. dim. _GE_ log. dim. (np .ge. n)
      dimension a(np,np),indx(np),b(np)

      do i=1,n      ! inicializo a cero la matriz A y b.
      
        do j=1,n
          a(i,j)=0.0 
        end do
      
        b(i)=0.0  
      end do

* defino los elementos de la matriz A y la matriz terminos independientes
    
      i=1              ! fila 1
      a(i,1)=1.0
      a(i,2)=1.0
      
      b(i)=3.0

      i=2              ! fila 2
      a(i,1)=1
      a(i,2)=2
      
      b(i)=5.0

      i=3              ! fila 3
      a(i,2)=-1.0
      a(i,3)=3.0
      a(i,4)=2.0
      
      b(i)=15.0

      i=4              ! fila 4
      a(i,4)=4.0
      a(i,5)=1.0
      
      b(i)=21.0

      i=5              ! fila 5
      a(i,4)=-1.0
      a(i,5)=2.0
      b(i)=6.0
      
* escribo los elementos de la matriz "A" y del vector "b"


      open(10,file='2-51-SolSist-app.dat',status='unknown')
    
      do i=1,n
        write(10,*) (a(i,j), j=1,n),' ||', b(i)
      end do
      
      write(*,*)'las matrices A  ||  b'
      
      do i=1,n
        write(*,*) (a(i,j), j=1,n),' ||', b(i)
      end do

* llamo a la subrutina que descompone la matriz "A" en el
* producto de matrices "LU"
      call ludcmp(a,n,np,indx,d)

* llamo a la subrutina que resuelve el sistema de ecuaciones
      call lubksb(a,n,np,indx,b)

      write(10,*)
      write(10,*) 'solucion:'
* escribo el vector columna con las soluciones
      write(10,1000) (b(i),i=1,n)
      close(10)
      
      write(*,*)'la solucion es:'
      write(*,*)
      write(*,1000) (b(i),i=1,n)
      
     
      stop

1000  format(5(2x,g14.7))
     
      end

