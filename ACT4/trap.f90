program trapezoid2

  implicit none
  integer:: n
  real :: u, a, b, integrand
  integer :: i

  WRITE(*,*)'Ingresar el intervalo de evalución'
  READ(*,*) a, b
  WRITE(*,*)'Ingrese el número de iteraciones'
  READ(*,*) n
  
  do i=0,n
     u = (i*(b - a))/n + a
     write (*,*) u, integrand(u)
  end do

  
    function integrand(x) result (value)
      implicit none
      real :: x
      real :: value

      if (x .lt. 0.00001) then
         x = 0.00001
      end if

      value = x**2
    end function integrand

end program trapezoid2
