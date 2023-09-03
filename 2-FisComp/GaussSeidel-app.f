*     GaussSeidel-master-rgm.f
*
* ESTO NO ES UNA SUBRUTINA - NO SE PUEDE USAR EN EL EXAMEN
*
* PROGRAMA "MASTER" PARA RESOLVER UN SISTEMA DE ECUACIONES LINEALES
* POR EL METODO DE GAUSS-SEIDEL

************************************************************************
*  es importante el orden en que se escriben las ecuaciones; para asegurarse 
*  de que funcione el programa, hay que reordenar las ecuaciones originales
*  de manera que la matriz de coeficientes sea diagonalmente dominante 
*
*     Rafael Garcia Molina           4.03.2008
************************************************************************

      subroutine gauss_seidel(a,b,n,kmax,tol,k,x)

      dimension a(n,n),b(n),x(n)



      do k=1,kmax    ! COMIENZAN LAS ITERACIONES
        difmax=0.0   ! inicializo a cero la diferencia maxima entre valores...
                     ! ...de las incognitas obtenidos en iteraciones consecutivas
        do i=1,n     ! recorrido por las filas (las ecuaciones)
          sum=0.0
          do j=1,i-1  ! recorrido por las columnas de cada fila
            sum=sum+a(i,j)*x(j)
          end do
          do j=i+1,n  ! recorrido por las columnas de cada fila
            sum=sum+a(i,j)*x(j)
          end do
          xx=(b(i)-sum)/a(i,i) ! "x" recien calculada
          dif=abs(xx-x(i)) ! error (absoluto) de cada solucion: comparo con valor anterior
          difmax=max(difmax,dif) ! me quedo con el maximo valor de cada diferencia
          x(i)=xx   ! actualizo las "x" recien calculadas
        end do      ! finaliza recorrido por las filas (las ecuaciones)

        if(difmax.le.tol) then  ! compruebo si se alcanza la convergencia
* escribo la iteracion "k" y las soluciones "x(i)" en cada iteracion
          write(*,*) 'se alcanzo la convergencia: tol=',tol,' con ',k,
     &    ' iteraciones'
          write(*,*)
          write(*,*) 'x=',(x(i),i=1,n)
          return
        end if
      end do   ! FINALIZAN LAS ITERACIONES

      write(*,*) 'no alcanzo la convergencia: tol=',tol,' con ',kmax, 
     &'iteraciones'
      write(*,*)'x=',(x(i),i=1,n)
      write(*,*)
      write(*,*) 'programa finalizado'
      
      return
     
      end
