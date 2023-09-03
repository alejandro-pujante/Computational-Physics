*     2-41bLU-inversa-rgm.f
************************************************************************
*  calculo del determinante y de la matriz inversa de la matriz formada
*  por los coeficientes de las incognitas de un sistema de ecuaciones
*  lineales ("n" ecuaciones con "n" incognitas)
*  por el metodo de descomposicion LU
*     Rafael Garcia Molina          8.03.2006
************************************************************************
      include '../ludcmp.f'
      include '../lubksb.f'
      parameter (n=5, np=5)   ! phys. dim. _GE_ log. dim. (np .ge. n)
      dimension a(np,np),indx(np),b(np)
      dimension y(np,np),ai(np,np),aa(np,np),c(np,np)
* defino los elementos de la matriz de coeficientes y el termino independiente
      i=1  ! fila 1
      a(i,1)=1.0
      a(i,2)=-2.0
      a(i,3)=0.0
      a(i,4)=-1.0
      a(i,5)=0.0

      i=2  ! fila 2
      a(i,1)=-2.0
      a(i,2)=7.0
      a(i,3)=1.0
      a(i,4)=0.0
      a(i,5)=-1.0

      i=3   ! fila 3
      a(i,1)=0.0
      a(i,2)=1.0
      a(i,3)=2.0
      a(i,4)=1.0
      a(i,5)=2.0

      i=4   ! fila 4
      a(i,1)=-1.0
      a(i,2)=0.0
      a(i,3)=-1.0
      a(i,4)=6.0
      a(i,5)=1.0

      i=5   ! fila 5
      a(i,1)=0.0
      a(i,2)=-1.0
      a(i,3)=2.0
      a(i,4)=1.0
      a(i,5)=9.0
      b(i)=0.0
      
* compruebo que he introducido bien los datos
      open(10,file='2-41bLU-inversa-rgm.dat',status='unknown')
      do i=1,n
        write(10,*) (a(i,j), j=1,n)    ! ,' ||', b(i)
      end do

* guardo copia de la matriz original para luego multiplicarla por la matriz inversa
      do i=1,n
        do j=1,n
          aa(i,j)=a(i,j)    ! aa = copia de seguridad de la matriz a
        end do
      end do

* defino los elementos de la matriz identidad
      do i=1,n
        do j=1,n
          y(i,j)=0.0  ! todos los elementos son nulos...
        end do
        y(i,i)=1.0    ! ...excepto los de la diagonal principal
      end do

      call ludcmp(a,n,np,indx,d)  ! descomposicion LU de la matriz A original

* calculo el determinante de la matriz A
      prod=1.0    ! inicializo el productorio: prod=1
      do i=1,n
        prod=prod*a(i,i)  ! productorio de la diagonal principal
      end do
      det=d*prod  ! multiplico productorio por paridad de permutaciones de ecuaciones
      write(10,*)
      write(10,*) 'det = ', det  ! escribo el determinante

      do j=1,n   ! construyo los elementos de cada columna
        do i=1,n
          b(i)=y(i,j)  ! el vector de terminos independientes es cada columna de A
        end do
        call lubksb(a,n,np,indx,b) ! solucion del sistema para cada vector columna
        do i=1,n
          ai(i,j)=b(i) ! matriz inversa formada por vectores columna solucion del sistema
        end do
      end do

      write(10,*)
      write(10,*) 'A^(-1) = '   ! escribo la matriz inversa
      do i=1,n
        write(10,1000) (ai(i,j),j=1,n)
      end do
      write(10,*)

      do i=1, n   ! compruebo si matriz inversa multiplicada por matriz original... 
        do j=1,n  ! ...da matriz identidad
          sum=0.0
          do k=1,n
            sum=sum+aa(i,k)*ai(k,j)
          end do
          c(i,j)=sum
        end do
      end do

      write(10,*)
      write(10,*) 'A * A^(-1) = '  ! escribo el producto A * A^(-1)
      do i=1,n
        write(10,1000) (c(i,j), j=1,n)
      end do
      close(10)
      write(*,*) 'programa finalizado'
      stop
1000  format(5(3x,g9.2))
      end
