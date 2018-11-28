
Program Biseccion

  IMPLICIT NONE

  REAL :: a, b, c, e

  PRINT*,'Introduce el intervalo donde se encuentra la raiz'
  READ(*,*) a, b

  IF (a>b) THEN
     
     a = b
     else
     b = a

end if

PRINT*, a, b

end program 

  
