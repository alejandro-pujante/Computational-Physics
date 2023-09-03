*      7-34-app.f
************************************************************************
*  Random walk para calles de 1 m de longitud
*  Distribucion uniforme entre 0.5 y 0.7
*  Distribucion exponencial
*
*     Alejandro Pujante Perez        20.05.2015
************************************************************************
      include '../ran2.f'      ! generador de numeros aleatorios
      parameter (N=1000)
      real mu
                               ! num =numeros aleatorios a generar

      pi=acos(-1.0)     ! defino el numero pi
      iseed= -595815    ! semilla para iniciar los calculos (puede ser cualquier numero)
 
      
      open(10,file='7-34-a-app.dat')   ! abro fichero donde guardare los datos del apartado a

      do i=1,N            ! i recorre los valores de N
        dmed2 = 0         ! inicio a 0 el contador donde almacenare las distancias al cuadrado   
        do k = 1 , 300    ! para cada N hago 300 'simulaciones'
            dx = 0.0
            dy = 0.0
            do j =1 , i   ! cada simulacion consta de recorrer 'N' calles y luego hacer la media
                  x=ran2(iseed)    ! el valor de x sera aleatorio
            
                  if(x.lt.0.25)  dx=dx+1.0                    ! se elige la direccion derecha    ! estas probabilidades tienen un 25% cada una
                  if(x.gt.0.25 .and. x.lt.0.5)  dy=dy+1.0     ! se elige la direccion arriba
                  if(x.gt.0.5 .and. x.lt.0.75) dx=dx-1.0      ! se elige la direccion izquierda
                  if(x.gt.0.75) dy=dy-1.0                     ! se elige la direccion abajo
            end do
            d2=dx**2+dy**2
            dmed2 = dmed2 + d2
        end do
        drms1 = sqrt(dmed2 / 300.0)       ! al terminar las simulaciones se hace la media
        write(10,*) i , drms1
        
      end do
      
      close(10)
      
************************************************************************
* ahora la direccion no esta prefijada , el angulo varia de 0 a 2pi
* y la longitud varia entre 0.5 y 0.7 m

      a=0.5
      b=0.7
      
      open(20,file='7-34-b-app.dat')

      do i=1,N
           dmed2=0.0
           do k = 1 , 300
              dx = 0.0
              dy = 0.0
              do j = 1 , i                                              
                  r=a + (b-a)*ran2(iseed)  !en sentido antihorario
                  angulo= 2*pi*ran2(iseed)
                  dx= dx + r*cos(angulo)
                  dy= dy + r*sin(angulo)                           ! el radio es una distancia minima de 0.5 mas algo que es como mucho 0.2 para no pasarse de 0.7                                                        ! el angulo estara entre 0 y 2pi                                                ! se descompone en los ejes cartesianos para obtener la posicion x ,y
              end do
              d2=dx**2+dy**2
              dmed2 = dmed2 + d2
           end do
        drms2 = sqrt(dmed2 / 300.0)
        write(20,*) i , drms2
      end do
      close(20)

************************************************************************
* ahora tendremos cualquier direccion entre 0 y 2pi
* pero la longitud o el radio ya no es uniforme sino esta dado por una distribucion exponencial
               
      mu = 0.6      ! media de la distribucion
      gamma= 1.0/mu ! la desviacion es la inversa
      
      open(30,file='7-34-c-app.dat')
      
      do i=1,N
           dmed2 = 0.0                   
            do k =1 , 300
                  dx = 0.0
                  dy = 0.0
                  do j=1 , i
                     r= -log(1.0- ran2(iseed)) / gamma       ! la longitud del radio es la distribucion exponencial
                     angulo=2*pi*ran2(iseed)
                     dx=dx + r*cos(angulo)
                     dy=dy + r*sin(angulo)
                  end do
                d2=dx**2+dy**2
                dmed2 = dmed2 + d2
            end do
        drms3 = sqrt(dmed2 / 300.0)
        write(30,*) i , drms3
      end do
      close(30)
      
      

************************************************************************      
      write(*,*) 'programa finalizado'
      
      stop
      end
