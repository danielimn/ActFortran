program trapezoid2

  implicit none
  integer :: n
  real :: u, a, b
  integer :: i

  WRITE(*,*)'Ingrese el intervalo a evaluar'
  READ(*,*) a, b
  WRITE(*,*)'Ahora ingrese el n�mero de particiones'
  READ(*,*) n

  
  

  call trapezoid_integration(n,a,b)

  contains

    subroutine trapezoid_integration(n,start_val,end_val)
      implicit none
      integer :: n
      real :: start_val,  end_val
      real :: integral,u,h
      integer :: i

     

      do i=0,n
         u = (i*(end_val-start_val))/n + start_val

         ! implement Eqn G.4
         if ((i.eq.0).or.(i.eq.n)) then
            integral = integral+integrand(u)
         else
            integral = integral+(2.0*integrand(u))
         end if

      end do

      h=(end_val-start_val)/n
      integral = (h/2.0)*integral

      write (*,*) '#trapezoidal integration = ',integral
    end subroutine trapezoid_integration

    function integrand(x) result (value)
      implicit none
      real :: x
      real :: value

      if (x .lt. 0.00001) then
         x = 0.00001
      end if

      value = x**4
    end function integrand
  

end program trapezoid2
