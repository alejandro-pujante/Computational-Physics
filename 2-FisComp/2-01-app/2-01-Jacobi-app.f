*     2-01Jacobi-rgm.f
************************************************************************
*   Solucion de un sistema de ecuaciones lineales
*   ("n" ecuaciones con "n" incognitas) por el metodo de Jacobi
*   Es importante el orden en que se escriben las ecuaciones para que
*   la matriz de coeficientes sea diagonalmente dominante
*
*   Como no es recomendable usar el metodo de Jacobi, no me preocupo
*   de prepararlo para que las iteraciones finalicen cuando el resultado
*   converja (los programas 2-01GaussSeidel-rgm.f y 2-01SOR-rgm.f
*   si que tienen un criterio de convergencia)
*
*   Tampoco preparo una subrutina para resolver sistemas de ecuaciones
*   mediante el metodo de Jacobi
*
*               9.03.2008    Rafael Garcia Molina
************************************************************************
      parameter (n=3) ! tamanyo de la matriz (n x n)
      dimension a(n,n),b(n),x(n),xold(n)

* inicializo a cero todo para evitar escribir luego los elementos nulos
      do i=1,n
        do j=1,n
          a(i,j)=0.0  ! inicializo a cero los elementos de la matriz
        end do
        b(i)=0.0      ! inicializo a cero los terminos independientes
      end do

* otra forma de definir todos los elementos nulos... 
*      a=0.0    !... de la matriz
*      b=0.0    !... del vector de terminos independientes

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
      
* escribo los elementos de la matriz "a" y del vector "b"
* para comprobar que he introducido bien los datos
      do i=1,n
        write(*,*) (a(i,j), j=1,n),' ||', b(i)
      end do
      write(*,*)

      kmax=200     ! kmax= numero maximo de iteraciones
* valores propuestos para iniciar la iteracion
      do i=1,n
        xold(i)=1.0
      end do

      open(10,file='2-01Jacobi-rgm.dat')
      write(*,*) '    k    x1            x2            x3'
      write(10,*) '    k    x1            x2            x3'

      do k=1,kmax  ! comienzan las iteraciones
        do i=1,n   ! recorrido por las filas (las ecuaciones)
          sum=0.0
          do j=1,i-1  ! recorrido por las columnas de cada fila
            sum=sum+a(i,j)*xold(j)
          end do
          do j=i+1,n  ! recorrido por las columnas de cada fila
            sum=sum+a(i,j)*xold(j)
          end do

* ESTA FORMA MAS SIMPLE DE EVITAR SUMAR EL TERMINO CON a(i,i) ES MAS COSTOSA
* EN TERMINOS DE CALCULO, PUES SIEMPRE TIENE QUE PREGUNTAR PARA EVITAR ESE TERMINO
*          do j=1,n  ! recorrido por las columnas de cada fila
*            if(j.ne.i) sum=sum+a(i,j)*xold(j) ! no uso "x" recien calculadas
*          end do
          x(i)=(b(i)-sum)/a(i,i)
        end do
        write(*,1000) k, (x(i), i=1,n)  ! escribo las "x" obtenidas
        write(10,1000) k, (x(i), i=1,n)  ! escribo las "x" obtenidas

* una vez finalizado el recorrido por todas las filas, actualizo los
* valores de las "xold" que voy a usar en la siguiente iteracion
        do i=1,n
          xold(i)=x(i)
        end do
      end do
      close(10)
      write(*,*)
      write(*,*) 'programa finalizado'
      stop
1000  format(2x,i4,5(1x,g13.7))
      end
