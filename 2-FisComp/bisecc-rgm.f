*     bisecc-rgm.f
************************************************************************
* subrutina para calcular ceros por el metodo de biseccion
* (se pasa la funcion "f" a traves del argumento de la subrutina)
*
* trabaja en precision simple
*
*     Rafael Garcia Molina       25.02.2008
************************************************************************
      subroutine bisecc(f,xl,xr,kmax,tol,k,xm)
* f = funcion cuyo cero se busca (se pasa la funcion "f" a traves del argumento)
* xl, xr = valores usados para acotar inicialmente el cero
* kmax = numero maximo de iteraciones
* tol = error absoluto (o relativo, segun se decida) con el que se obtiene el resultado
* xm = valor obtenido para el cero en la iteracion k
      external f
      real f
      fl=f(xl)
      fr=f(xr)
* antes de hacer calculos, comprobamos si los valores iniciales acotan bien el cero buscado
      if(fl*fr .gt. 0.0) goto 100  ! si fl y fr tienen el mismo signo no se busca el cero
      do k=1,kmax
        xm=(xr+xl)/2.0             ! x: valor medio
*        xm=(xr*fl-xl*fr)/(fl-fr)  ! x: falsa posicion  (NO LA USO NUNCA)
        fm=f(xm)
*        write(*,*) k,xm,dif  ! escribo en pantalla la evolucion de la solucion
        if (fm*fr .le. 0.0) then
          xl=xm
*          fl=fm    ! realmente no es necesario definir redefinir fl,...
                    ! ...pues no se usa en la comparacion  del "if"
        else
          xr=xm
          fr=fm
        end if
        dif=abs(xr-xl)            ! criterio de error absoluto
*        dif=abs((xr-xl)/xm)        ! criterio de error relativo
        if(dif .lt. tol) goto 200     ! se alcanza la convergencia y finaliza el calculo
      end do
*      write(*,*) 'num. iter.=',kmax,'   x=',xm
      write(*,*) 'no se alcanzo la precision deseada'
      return
200   write(*,*) 'se alcanzo la precision deseada'
      return
100   write(*,*) 'datos iniciales mal elegidos'
      return
      end
