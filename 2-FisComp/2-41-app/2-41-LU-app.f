*     2-41bLU-rgm.f
************************************************************************
*   solucion de un sistema de ecuaciones lineales
*   ("n" ecuaciones con "n" incognitas)
*   por el metodo de descomposicion LU
*     Rafael Garcia Molina          31.05.2005
************************************************************************
      include '../ludcmp.f'
      include '../lubksb.f'
      parameter (n=5,np=5)     ! phys. dim. _GE_ log. dim. (np .ge. n)
      dimension a(np,np),indx(np),b(np)

      do i=1,n  ! inicializo a cero...
        do j=1,n
          a(i,j)=0.0 !...todos los elementos de la matriz "a" de coeficientes
        end do
        b(i)=0.0  !...todos los elementos del vector "b" de terminos independientes
      end do

* defino los elementos de la matriz de coeficientes y el termino independiente
      i=1  ! fila 1
      a(i,1)=1.0
      a(i,2)=-2.0
      a(i,4)=-1.0
      b(i)=3.0

      i=2  ! fila 2
      a(i,1)=-2.0
      a(i,2)=7.0
      a(i,3)=1.0
      a(i,5)=-1.0
      b(i)=2.0

      i=3   ! fila 3
      a(i,2)=1.0
      a(i,3)=2.0
      a(i,4)=1.0
      a(i,5)=2.0
      b(i)=1.0

      i=4   ! fila 4
      a(i,1)=-1.0
      a(i,3)=-1.0
      a(i,4)=6.0
      a(i,5)=1.0
      b(i)=4.0

      i=5   ! fila 5
      a(i,2)=-1.0
      a(i,3)=2.0
      a(i,4)=1.0
      a(i,5)=9.0
      b(i)=0.0
      
* escribo los elementos de la matriz "a" y del vector "b"
* para comprobar que he introducido bien los datos
      open(10,file='2-41bLU-rgm.dat',status='unknown')
      do i=1,n
        write(10,*) (a(i,j), j=1,n),' ||', b(i)
      end do

* llamo a la subrutina que descompone la matriz "a" en el
* producto de matrices "l*u"
      call ludcmp(a,n,np,indx,d)

* llamo a la subrutina que resuelve el sistema de ecuaciones
      call lubksb(a,n,np,indx,b)

      write(10,*)
      write(10,*) 'solucion:'
* escribo el vector columna con las soluciones
      write(10,1000) (b(i),i=1,n)
      close(10)
      write(*,*) 'programa finalizado'
      stop
1000  format(5(2x,g14.7))
      end
