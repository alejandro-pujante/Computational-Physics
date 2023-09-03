*     0-03c-app.f
************************************************************************
*   programa para calcular area y volumen de un cilindro   
*   los datos se leen en un fichero .in y se escriben en otro .dat
*
*     Alejandro Pujante Pérez             21.02.2021
************************************************************************

        pi = acos(-1.0) !Defino el valor de pi
        
*Lectura de los datos

        open(10, file = '0-03c-app.in' , status = 'unknown') !Le digo que abra la carpeta 10 (arbitraria) y el archivo 0-03c-app.in
        
        read(10,*) r                                         ! Lee dentro de la carpeta 10 el dato 'r'
        read(10,*) h                                         ! Lee dentro de la carpeta 10 el dato 'r'
        
        close(10)                                            ! Cierra la carpeta 10
        
*Defino las superficies

        abase = pi*r**2              ! area de la base
        alado = 2.0*pi*r*h           ! area del lado
        
        atotal = 2*abase + alado     ! area total = 2 bases + lado
        
*Calculo del volumen
        
        v = abase * h                ! volumen total del cilindro
        
        
*Escritura de los resultados

        open(20 , file = '0-03c-app.dat' , status = 'unknown') !Le digo que en la carpeta 20(arbitraria) escriba los datos en 0-03c-app.dat
        write(20,*) 'r=' , r , 'm' , 'h=' , h , 'm'
        write(20,*) 'area=' , atotal , 'm**2' , 'volumen=' , v , 'm**3'
        close(20)
        write(*,*) 'programa finalizado'
        
        stop 
        
        
        
        end
        
