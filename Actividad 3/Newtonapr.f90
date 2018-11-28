Program NewtonRap

  IMPLICIT NONE
  
  !Declaramos las siguientes variables
  !x: ser� una mejor a proximaci�n a la raiz
  !x1: un punto en el eje x cercano a la raiz
  !error: corresponde a un porcentaje deseado para la exactitud de la raiz
  !f: es para la funci�n que se desea aproximar su raiz
  !f1: es la derivada de f
  
  REAL:: x,x1,error,f,f1
  
  !Pedimo al usario ingrese nuestro punto cercano a la raiz x1 y lo leemos
  WRITE(*,*)'Ingresa un punto cerca de la raiz'
  READ(*,*) x1
  
  !Usaremos un loop para llevar acabo la operaci�n las veces necesarias
  !El loop ser� deteneido por un if en donde se muestra que el error debe de ser
  !menor a un porcentaje ya se�alado
  DO
     !Se evalua la funci�n y su derivada en x1
     f=x1**3-x1-2
     f1=3*x1**2-1
     !Aqu� tenemos el m�todo de Newton que nos da una mejor aproximaci�n a
     !nuestra raiz con la siguiente operaci�n
     x=x1-(f/f1)
     !Definimos el error
     error= abs((x-x1)/x)*100
     x1=x
     !Si este if cumple con lo se�alado entonces se detendr� el loop, de otra
     !forma ser�a infinito
     if(error<0.0000001)exit
  END DO
  
  !Imprimimos en pantanlla el valor de x correspondiente a la raiz
  PRINT*,'La raiz es:',x

  END PROGRAM NewtonRap

 
    
     
