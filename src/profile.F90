!===============================================================================
!>
!-------------------------------------------------------------------------------
MODULE profile_mod

  IMPLICIT NONE
  PRIVATE


  !=============================================================================
  !> A single profile observation.
  !! The level based values (depth, salt, temp) should all have the same array
  !! size. If a profile does not have a given oceanic variable, that variable
  !! will be allocated with size 0 here. (e.g. if a profile has temperature only
  !! and no salinity, SIZE(salt) will return 0, and SIZE(temp) == SIZE(depth) )
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: profile
     CHARACTER(8) :: id !< callsign of the platform
     REAL(8) :: lat     !< latitude (degrees)
     REAL(8) :: lon     !< longitude (degrees)
     INTEGER :: date    !< year, month, day (YYYYMMDD)
     REAL :: hour       !< time of day (fractional hours)
     REAL, ALLOCATABLE :: depth(:)  !< depth of each level (m)
     REAL, ALLOCATABLE :: salt(:)   !< salinity of each level (PSU)
     REAL, ALLOCATABLE :: temp(:)   !< temperature (C)

   CONTAINS

     PROCEDURE :: PRINT => profile_print

  END TYPE profile
  !=============================================================================


  ! used in the salt or temp variables to indicate there is no recorded
  ! value for that level
  REAL, PARAMETER, PUBLIC :: PROF_UNDEF = 1.0e10


CONTAINS


  SUBROUTINE profile_print(self)
    CLASS(profile), INTENT(in) :: self
    INTEGER :: i

    REAL :: v1, v2
    PRINT *, "id:   ", self%id
    PRINT *, "date: ", self%date
    PRINT *, "hour: ", self%hour
    PRINT *, "lat:  ", self%lat
    PRINT *, "lon:  ", self%lon

    DO i =1, SIZE(self%depth)
       v1 = MERGE(self%temp(i), PROF_UNDEF, SIZE(self%temp)>=i)
       v2 = MERGE(self%salt(i), PROF_UNDEF, SIZE(self%salt)>=i)
       PRINT '(I5, F8.2, F10.3, F10.3)', i, self%depth(i), v1, v2
    END DO

  END SUBROUTINE profile_print


END MODULE profile_mod
!===============================================================================




!===============================================================================
!> Templated vector object for holding profile types
!-------------------------------------------------------------------------------
MODULE vec_profile_mod
  USE profile_mod
#define _type type(profile)
#define _vector vec_profile
#include "templates/vector.inc"
END MODULE vec_profile_mod
!===============================================================================
