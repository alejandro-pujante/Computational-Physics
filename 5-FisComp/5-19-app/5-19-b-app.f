*     5-19-b-LU-app.f
************************************************************************
*  funciones de onda radiales para los orbitales 1s y 3s
*  
*
*     Alejandro Pujante Perez              9.05.2021
************************************************************************

        external f
        real f
        real a0 , z , qe
        parameter (npasos=10000)
        
        a0 = 1.0
        z = 9.0
        qe = 1.0
        
        
        x0 = 0.0
        xf = 10.0
        
        
        h = (xf-x0)/real(npasos)
        
        open(10,file='5-19-den1s-app.dat')
        
        write(10,*)'x      fx'
        
        do k = 1 , npasos
        
            x = x0 + k*h
            
            write(10,*) x , f(x)
            
        end do
        
        close(10)
        
        
        open(20,file='5-19-den3s-app.dat')
        
        write(20,*)'x      fx'
        
        do k = 1 , npasos
        
            x = x0 + k*h
            
            write(20,*) x , g(x)
            
        end do
        
        close(20)
        
        
        write(*,*)'programa finalizado'
        
        
        end
        
                
************************************************************************
        function f(x)
************************************************************************

        real a0 , z , qe
        
        a0 = 1.0
        z = 9.0
        qe = 1.0
        
        f = x**2*4.0*exp(-2.0*z*x/a0)*(z/a0)**3

        return
        end
        
************************************************************************
        function g(x)
************************************************************************

        real a0 , z , qe
        
        a0 = 1.0
        z = 9.0
        qe = 1.0


        g=x**2*(4.0*(z/3.0)**3*abs(1.0-(2.0/3.0)*(z*x)+(2.0/27.0
     & )*(z*x)**2)**2*exp(-2.0*z*x/3.0))
        
        
        return
        end
        
