!===============================================================================
!>
!-------------------------------------------------------------------------------
MODULE profile_mod

  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC :: profile
     REAL(8) :: lat
     REAL(8) :: lon
     REAL :: time
     REAL, ALLOCATABLE :: depth(:)
     REAL, ALLOCATABLE :: salt(:)
     REAL, ALLOCATABLE :: temp(:)
  END TYPE profile

END MODULE profile_mod
!===============================================================================




!===============================================================================
!>
!-------------------------------------------------------------------------------
MODULE vec_profile_mod
  USE profile_mod
#define _type type(profile)
#define _vector vec_profile
#include "templates/vector.inc"
END MODULE vec_profile_mod
!===============================================================================
