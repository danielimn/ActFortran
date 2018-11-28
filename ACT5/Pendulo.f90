PROGRAM PENDULO
  IMPLICIT NONE

  REAL :: a = 1.0, b = 2.0, x = -4.0
  REAL :: h, t, f
  INTEGER :: n = 100
  INTEGER :: k

    h = (b-a)/real(n)
    t = a

  PRINT*, t, x

  DO k = 1, n

     x = x + h*f(t,x)
     t = t + h

     PRINT*, 'Estos son k, t, x: ', k, t, x

  END DO

END PROGRAM PENDULO

  FUNCTION f(t,x)
    REAL,intent(in):: x, t
    f = 1.0 + x*x + t**3
  END FUNCTION f



  
