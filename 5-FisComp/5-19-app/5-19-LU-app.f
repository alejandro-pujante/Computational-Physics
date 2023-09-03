*     5-19-1S-LU-app.f
************************************************************************
*  solucion a la ecuacion diferencial de poisson para la funcion
*  de onda radial R10 por el metodo de diferencias finitas
*
*
*     Alejandro Pujante Perez              9.05.2021
************************************************************************
    
      include '../ludcmp.f'
      include '../lubksb.f'
      parameter (npto=500)  
      parameter (n=npto-1, np=npto-1) 
      dimension a(np,np),indx(np),b(np)
      x0=0.01        ! valor inicial del dominio
      xf=400.0       ! valor final del dominio
      y0= -9.0       ! condicion de contorno en x0
      yf=0.0           
      h=(xf-x0)/real(npto)  
      
      z = 9.0
      a = 1.0
      qe = -1.0

* inicializo a cero todos los elementos de la matriz de coeficientes
* y del vector de terminos independientes
      do i=1,n
        do j=1,n
          a(i,j)=0.0
        end do
      end do

      hh=h**2
    
      do j=1,n                  ! diagonal principal
        x=x0+j*h
        a(j,j)= -2.0/hh
      end do
*      write(*,*)

      do j=1,n-1  
          x = x0+j*h                  ! diagonal por encima de la principal
         a(j,j+1)=1.0/(h*x) + 1.0/hh
      end do

      do j=2,n
         x = x0 + j*h                   ! diagonal por debajo de la principal
         a(j,j-1)= -1.0/(h*x) + 1.0/hh
      end do

* defino los elementos del vector de terminos independientes

      b(1)= 4.0*z**(3)*exp(-2.0*z*(x0+h))-y0
     
      do j=2,n-1
        x=x0+j*h
        b(j)= 4.0*z**(3)*exp(-2.0*z*x)
 
      end do

      b(n)= 4.0*z**(3)*exp(-2.0*z*(xf-h)) - yf



      call ludcmp(a,n,np,indx,d)    
      call lubksb(a,n,np,indx,b)


      open(10,file='5-19-LU-app.dat',status='unknown')

      write(10,*) x0,y0 
                                    ! valores al principio de la region
      do j=1,n
        x=x0+j*h
        write(10,*) x,b(j)          ! valores dentro de la region
      end do
      write(10,*) xf,yf             ! valores al final de la region
      close(10)

      write(*,*) 'programa finalizado'
      stop
      end
