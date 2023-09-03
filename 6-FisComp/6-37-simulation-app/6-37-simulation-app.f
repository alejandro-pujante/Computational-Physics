********************************************************
* 6-37.f
* Noel Gomariz Kühne, 48751186H
* mis muertos mis muertos mis muertos mis muertos
* mis muertos mis muertos mis muertos mis muertos
* mis muertos mis muertos mis muertos mis muertos
* mis muertos mis muertos mis muertos mis muertos
* 14/05/2021
********************************************************

      implicit real*8 (a-h, o-z)
      parameter (Nx = 1000, Nt = 1400)
      dimension u(Nx), uold(Nx), unew(Nx)
      real*8 L, m1, m2, lamb1, lamb2

      m1 = 200.0d0 
      m2 = 300.0d0 
      L = 500.0d0 

      T = 300.0d0 

      lamb1 = 2.0d0 * m1/L
      lamb2 = 2.0d0 * m2/L

      c1 = dsqrt(T / lamb1)
      c2 = dsqrt(T / lamb2)

      hx = L / dble(Nx)
      ht = hx/c1

      rat1 = (c1*ht/hx)**2
      rat2 = (c2*ht/hx)**2

      x0 = 0.0d0
      x = x0

      ! Forma inicial de la cuerda
      u(1) = 0.0d0
      uold(1) = 0.0d0
      unew(1) = 0.0d0
      u(Nx) = 0.0d0
      uold(Nx) = 0.0d0
      unew(Nx) = 0.0d0

      write(*, *) 'ht = ', ht, ', hx/c1 =', hx/c1, ', hx/c2 =', hx/c2
      
      do i = 2, Nx-1
          x = x+hx
          uold(i) = 1.0d-4 * x**4/(dexp(2.0d-2 * x**2) - 1)
      end do


      ! Segundo instante de tiempo
      do i = 2, Nx/2 - 1
          u(i) = 0.5d0 * rat1 * (uold(i-1) + uold(i+1)) +
     &     (1.0d0 - rat1) * uold(i) 
      end do

      do i = Nx/2, Nx-1
          u(i) = 0.5d0 * rat2 * (uold(i-1) + uold(i+1)) +
     &     (1.0d0 - rat2) * uold(i) 
      end do

      open(1, file='6-37-simulation-app.dat')

      write(1, *) (uold(i), i = 1, Nx)
      write(1, *) (u(i), i = 1, Nx)


      ! Resto de iteraciones
      do k = 3, Nt
          do i = 2, Nx/2 - 1
              unew(i) = rat1 * (u(i-1) + u(i+1)) +
     &         2.0d0*(1.0d0 - rat1) * u(i) - uold(i)
          end do

          do i = Nx/2, Nx-1
              unew(i) = rat2 * (u(i-1) + u(i+1)) +
     &         2.0d0*(1.0d0 - rat2) * u(i) - uold(i)
          end do
          
          do i = 1, Nx
              uold(i) = u(i)
              u(i) = unew(i)
          end do

          write(1, *) (unew(i), i = 1, Nx)
          
      end do
      
      write(*,*)'programa finalizado'

      close(1)
      
      stop
      end
