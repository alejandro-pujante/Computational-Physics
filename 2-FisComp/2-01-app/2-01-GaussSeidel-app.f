*     2-01GaussSeidel-rgm.f
************************************************************************
*   Solucion de un sistema de ecuaciones lineales
*   ("n" ecuaciones con "n" incognitas) por el metodo de Gauss-Seidel
*
*   Es importante el orden en que se escriben las ecuaciones
*   para que la matriz de coeficientes sea diagonalmente dominante
*
*               4.03.2008    Rafael Garcia Molina
************************************************************************
      include '../GaussSeidel-app.f'
      parameter (n=3)   ! tamanyo de la matriz (n x n)
      dimension a(n,n),b(n),x(n)
     
      kmax=200     ! kmax= numero maximo de iteraciones
      tol=2.0e-9   ! tol = error (absoluto) de cada solucion
      do i=1,n  ! inicializo a cero todo y evito escribir luego los elementos nulos
        do j=1,n
          a(i,j)=0.0  ! inicializo a cero los elementos de la matriz
        end do
        b(i)=0.0      ! inicializo a cero los terminos independientes
      end do

* defino por filas:
* elementos de la matriz de coeficientes "a" y terminos independientes "b"
* asi es facil intercambiar filas (es decir, ecuaciones) cuando sea necesario
      i=1  ! fila 1
      a(i,1)=2.0
      a(i,3)=1.0   ! notese que no defino a(i,2)=0.0 porque ya lo he hecho antes
      b(i)=5.0

      i=2  ! fila 2
      a(i,1)=-1.0
      a(i,2)=2.0   ! notese que no defino a(i,3)=0.0 porque ya lo he hecho antes
      b(i)=3.0

      i=3  ! fila 3
      a(i,1)=1.0
      a(i,2)=1.0
      a(i,3)=1.0
      b(i)=6.0
      
* escribo los elementos de la matriz "a" y del vector "b" ...

      do i=1,n  ! ...para comprobar que he introducido bien los datos
        write(*,*) (a(i,j), j=1,n),' ||', b(i)
      end do
      write(*,*)


      do i=1,n ! valores propuestos para iniciar la iteracion
        x(i)=1.0
      end do
      
      
      call gauss_seidel(a,b,n,kmax,tol,k,x)
      
      
      write(*,*) '    k    x1            x2            x3'
      write(*,1000) k, (x(m), m=1,n)
      write(*,*)
      write(*,*) 'programa finalizado'
      stop

1000  format(2x,i4,5(1x,g13.7))

      end
