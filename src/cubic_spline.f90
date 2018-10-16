MODULE cubic_spline
  !! cubic spline class
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: cspline

  TYPE :: cspline
     REAL, ALLOCATABLE :: x(:)
     REAL, ALLOCATABLE :: y(:)
     REAL, ALLOCATABLE :: y2(:)
   CONTAINS
     PROCEDURE :: interp => cspline_interp
     PROCEDURE :: deriv  => cspline_deriv
  END TYPE cspline

  INTERFACE cspline
     MODULE PROCEDURE cspline_init
  END INTERFACE cspline

  REAL, PARAMETER, PUBLIC :: cspline_error = -9.99e30

CONTAINS



  !============================================================
  !============================================================
  FUNCTION cspline_init(x,y)
    ! TODO, allow specification of the derivative at the boundaries.
    ! for now only natural boundaries are possible, but this
    ! should take care of the majority of the situations
    REAL, INTENT(in) :: x(:), y(:)
    TYPE(cspline) :: cspline_init

    REAL :: u(SIZE(x)-1)
    REAL :: sig, p, qn, un
    INTEGER :: n, i

    n = SIZE(x)
    IF(n <= 1) THEN
       PRINT *, "ERROR: cspline_init() given array of size < 2"
       STOP 1
    END IF
    IF(SIZE(x) /= SIZE(y)) THEN
       PRINT *, "ERROR: cspline_init() given x,y of different sizes"
       STOP 1
    END IF


    ALLOCATE(cspline_init%y2(n))
    ALLOCATE(cspline_init%x(n))
    ALLOCATE(cspline_init%y(n))
    cspline_init%x = x
    cspline_init%y = y

    ! natural lower boundary condition
    cspline_init%y2(1) = 0.0
    u(1) = 0.0

    ! decomposition loop of the tridiagonal
    DO i=2, n-1
       sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
       p = sig*cspline_init%y2(i-1)+2.0
       cspline_init%y2(i) = (sig-1.0) / p
       u(i) = (y(i+1)-y(i))/(x(i+1)-x(i)) - &
            (y(i)-y(i-1))/(x(i)-x(i-1))
       u(i) = (6.0*u(i)/(x(i+1)-x(i-1))-sig*u(i-1))/p
    END DO

    ! natural upper boundary conditions
    un = 0.0
    qn = 0.0

    !backsubsitution
    cspline_init%y2(n) = (un-qn*u(n-1)) / (qn*cspline_init%y2(n-1)+1.0)
    DO i=n-1,1,-1
       cspline_init%y2(i) = cspline_init%y2(i)*cspline_init%y2(i+1)+u(i)
    END DO
  END FUNCTION cspline_init
  !============================================================



  !============================================================
  !============================================================
  PURE FUNCTION locate(this, x) RESULT(idx)
    CLASS(cspline), INTENT(in) :: this
    REAL, INTENT(in) :: x
    INTEGER :: idx

    INTEGER :: il, iu, im

    ! find the 2 x points the given x is between
    ! TODO, use the smarter algorithm in numerical recipes
    ! (hunt / locate)
    il = 1
    iu = SIZE(this%x)
    DO WHILE(iu-il > 1)
       im = (il+iu) / 2
       IF(x >= this%x(im)) THEN
          il=im
       ELSE
          iu = im
       END IF
    END DO
    idx = MAX(1,MIN(SIZE(this%x)-1,il))


  END FUNCTION locate
  !============================================================



  !============================================================
  !============================================================
  ELEMENTAL REAL FUNCTION cspline_interp(this, x, check) RESULT(y)
    CLASS(cspline), INTENT(in) :: this
    REAL, INTENT(in) :: x
    LOGICAL, OPTIONAL, INTENT(in) :: check
    REAL :: r

    REAL :: a,b,h
    INTEGER :: i1, i2


    ! ensure x is in the correct range
    IF(x < this%x(1) .OR. x > this%x(SIZE(this%x))) THEN
       y = cspline_error
       RETURN
    END IF

    ! find index
    i1 = locate(this, x)
    i2 = i1+1

    ! calculate value at given x
    h=this%x(i2)-this%x(i1)
    a=(this%x(i2)-x)/h
    b=(x-this%x(i1))/h
    y=a*this%y(i1)+b*this%y(i2)+&
         ((a*a*a-a)*this%y2(i1)+&
         (b*b*b-b)*this%y2(i2))*(h*h)/6.0

    !check to make sure interpolated value is not outside the range of
    ! the values above and below it, it if is switch to simple
    ! linear interpolation (useful for interpolating ocean profiles)
    IF(PRESENT(check) .AND. check) THEN
       IF ( y > MAX(this%y(i1),this%y(i2)) .OR. y < MIN(this%y(i1),this%y(i2))) THEN

          r = (x-this%x(i1)) / (this%x(i2)-this%x(i1))
          y = (this%y(i2)-this%y(i1)) * r + this%y(i1)
       END IF
    END IF
  END FUNCTION cspline_interp
  !============================================================



  !============================================================
  !============================================================
  ELEMENTAL REAL FUNCTION cspline_deriv(this, x) RESULT(y)
    CLASS(cspline), INTENT(in) :: this
    REAL, INTENT(in) :: x

    REAL :: a,b,h
    INTEGER :: i1, i2


    ! ensure x is in the correct range
    IF(x < this%x(1) .OR. x > this%x(SIZE(this%x))) THEN
       y = cspline_error
       RETURN
    END IF

    ! find index
    i1 = locate(this, x)
    i2 = i1+1

    ! calculate value at given x
    h=this%x(i2)-this%x(i1)
    a=(this%x(i2)-x)/h
    b=(x-this%x(i1))/h
    y=(this%y(i2)-this%y(i1))/h + &
         ((3*(b*b)-1)*this%y2(i2) - &
         (3*(a*a)-1)*this%y2(i1))*h/6.0

  END FUNCTION cspline_deriv
  !============================================================


END MODULE cubic_spline
