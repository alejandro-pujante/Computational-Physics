*   6-37-app.f
************************************************************************
*  Solucion cuerda con condiciones de contorno iniciales y
*  cogida por los extremos.
*
*
*   Alejandro Pujante Perez                   13.05.2021
************************************************************************


      implicit real*8 (a-h,o-z)
      parameter (nx=1000)               ! nx = numero de puntos en x
      parameter (nt=1000)               ! numero de pasos del tiempo
      dimension unew(0:nx),u(0:nx),uold(0:nx),uderiv(0:nx)
      character tiempo*5  
      real*8 masa1,masa2,long

************************************************************************
*     DATOS FISICOS DEL PROBLEMA  (trabajo en unidades SI)
************************************************************************
      tens=300.0d0             ! tension (dyn)
      masa1=200.0d0            ! masa (g)
      masa2=300.0d0
      long=500.0d0             ! longitud (metro)

************************************************************************
*     DATOS MATEMATICOS DEL PROBLEMA
***********************************************************************

      jframe=nt/5  ! jframe = cuantos pasos voy a escribir de la cuerda
************************************************************************
      rho1=masa1/(long/2.0d0)
      rho2=masa2/(long/2.0d0)      ! densidad masica lineal de la cuerda
      c1=sqrt(tens/rho1)           ! velocidad de la onda
      c2=sqrt(tens/rho2)
      x0=0.0d0                   ! limite inferior de x
      xf=long                    ! limite superior de x
      hx=(xf-x0)/dble(nx)        ! paso de x
      ht=0.003d0          
      ht1=(hx/c1)                ! valor maximo del paso temporal
      ht2=(hx/c2)
      write(*,*) 'ht=', ht , '   tiempo total=', nt*ht

      ratio1=(c1*ht1/hx)**2 
      ratio2=(c2*ht1/hx)**2


* CONDICIONES DE CONTORNO:
* los extremos de la cuerda estan fijos siempre 

      uold(0)=0.0d0    ! denominacion de la variable en el penultimo calculo
      uold(nx)=0.0d0   ! (tiempo = j-1)

      u(0)=0.0d0       ! denominacion de la variable en el ultimo calculo
      u(nx)=0.0d0      ! (tiempo = j)

      unew(0)=0.0d0    ! denominacion de la variable recien calculada
      unew(nx)=0.0d0   ! (tiempo = j+1)

      uderiv(0)=0.0d0  ! velocidad en los extremos..
      uderiv(nx)=0.0d0 ! ... que es nula en todo instante 


* CONDICION INICIAL (forma de la cuerda en el instante t=0)
      do i=1,nx-1  ! recorro la coordenada x (excepto los extremos, que estan fijos)
        x=x0+i*hx

        uold(i)=(0.0001d0*x**4)/(exp(2.0d0*(x/10.0d0)**2)-1)  ! inicialmente t=0
      end do

* escribo la forma inicial de la deformacion de la cuerda 
      open(1, file='6-37-00000-app.dat',status='unknown')
      write(*,*) '#   tiempo=', 0.0d0, ' s'
      do i=0,nx
        x=x0+i*hx
        write(1,*) x,uold(i)
      end do
      close(1)

* velocidad inicial de la deformacion de la cuerda (instante inicial: j=0)
      do i=1,nx-1          ! recorro la coordenada x (excepto los extremos, que estan fijos)
        uderiv(i)=0.0d0    ! velocidad inicial nula
      end do

* deformacion de la cuerda para el instante siguiente al inicial (j=1)
      do i=1,nx/2-1                     ! resuelvo la primera mitad
        u(i)=(ratio1/2.0d0)*(uold(i+1)+uold(i-1))+(1.0d0-ratio1)*uold(i)
     &       +ht1*uderiv(i)
      end do
      
      do i=nx/2,nx-1                    ! resuelvo la segunda mitad
        u(i)=(ratio2/2.0d0)*(uold(i+1)+uold(i-1))+(1.0d0-ratio2)*uold(i)
     &       +ht2*uderiv(i)
      end do


* a medida que transcurre cada incremento de tiempo "ht"
      do j=2,nt   
        do i=1,nx/2-1   ! resuelvo ka primera mitad
          unew(i)=ratio1*(u(i+1)+u(i-1))+2.0d0*(1.0d0-ratio1)*u(i) -    
     &     uold(i)
        end do
        do i=nx/2,nx-1 ! resuelvo la segunda mitad
          unew(i)=ratio2*(u(i+1)+u(i-1))+2.0d0*(1.0d0-ratio2)*u(i) -    
     &     uold(i)
        end do

        do i=1,nx-1
          uold(i)=u(i)
          u(i)=unew(i)
        end do

* escribo resultados 
        do jj=1,5      ! si el tiempo corresponde a un multiplo de 1/5 del total
          if (j.eq.jframe*jj) then
            write(tiempo,'(i5.5)') j  ! escribo en "tiempo" el instante "j" (en formato i5) en que se realiza el calculo
            open(40,file='6-37-'//tiempo//'-app.dat') ! escribo los resultados
            write(*,*) jj,'tiempo=', j*ht, ' s'
            do i=0,nx
              x=x0+i*hx
            write(40,*) x,u(i)
            end do
            close(40)
          end if
        end do
      end do
      write(*,*) 'programa finalizado'
      stop
      end
