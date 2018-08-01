MODULE profile_mod
  IMPLICIT NONE
  PRIVATE


  TYPE, PUBLIC :: profile
     REAL :: lat
     REAL :: lon
     REAL :: time
     REAL, ALLOCATABLE :: depth(:)
     REAL, ALLOCATABLE :: salt(:)
     REAL, ALLOCATABLE :: temp(:)
  END TYPE profile


END MODULE profile_mod
