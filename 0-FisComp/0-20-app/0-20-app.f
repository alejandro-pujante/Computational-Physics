*     0-20-app.f
************************************************************************
*   programa para comprobar que el cociente entre la masa del protón
*   y la del electrón coinciden con el valor de 6*pi**5
*
*   Alejandro Pujante Pérez           21.02.2021
************************************************************************

        real mp , me              !Declaro las variables mp , me reales
        
        pi = acos(-1.0)           !Defino el valor de pi
         
        mp = 1.6726219e-27        !Defino la masa del electron y ptroton
        
        me = 9.10938356e-31
        
        co = mp / me              !Hago el cociente entre los dos
        
        write(*,*)co , 6*pi**5    !Compruebo que son valores similares
        
        stop
        
        end
        
