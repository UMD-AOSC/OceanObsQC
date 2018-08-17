!===============================================================================
!> Defines the types needed to hold profiles
!-------------------------------------------------------------------------------
MODULE profile_mod

  IMPLICIT NONE
  PRIVATE



  !> platform types.
  !! Used within the profile%plat variable
  !-----------------------------------------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: PLAT_UNKNOWN = 0 !< Unknown platform type
  INTEGER, PUBLIC, PARAMETER :: PLAT_BATHY   = 1 !< bathythermographs (e.g. XBT)
  INTEGER, PUBLIC, PARAMETER :: PLAT_BUOY    = 2 !< buoys (moored, drifting)
  INTEGER, PUBLIC, PARAMETER :: PLAT_FLOAT   = 3 !< profiling floats (e.g. Argo)


  !> used in the profile%salt or profile%temp variables to indicate there is
  !!  no recorded value for that level
  REAL, PARAMETER, PUBLIC :: PROF_UNDEF = 1.0e10



  !=============================================================================
  !> A single profile observation.
  !! The level based values (depth, salt, temp) should all have the same array
  !! size. If a profile does not have a given oceanic variable, that variable
  !! will be allocated with size 0 here. (e.g. if a profile has temperature only
  !! and no salinity, SIZE(salt) will return 0, and SIZE(temp) == SIZE(depth) )
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: profile
     CHARACTER(10) :: id             !< unique ID for the platform / ship
     REAL(8) :: lat                  !< latitude (degrees)
     REAL(8) :: lon                  !< longitude (degrees)
     INTEGER :: date                 !< year, month, day (YYYYMMDD)
     REAL :: hour                    !< time of day (fractional hours)
     INTEGER :: plat                 !< platform type (see the PLAT_ variables)
     REAL, ALLOCATABLE :: depth(:)   !< depth of each level (m)
     REAL, ALLOCATABLE :: salt(:)    !< salinity of each level (PSU)
     REAL, ALLOCATABLE :: temp(:)    !< temperature (C)
   CONTAINS
     PROCEDURE :: PRINT => profile_print
  END TYPE profile
  !=============================================================================



CONTAINS



  !=============================================================================
  !> A human friendly profile summary. Mainly for debugging.
  !-----------------------------------------------------------------------------
  SUBROUTINE profile_print(self)
    CLASS(profile), INTENT(in) :: self
    INTEGER :: i

    CHARACTER(len=12) :: plat_type

    REAL :: v1, v2
    PRINT *, ""
    PRINT *, "id:   ", self%id
    PRINT *, "date: ", self%date
    PRINT *, "hour: ", self%hour
    PRINT *, "lat:  ", self%lat
    PRINT *, "lon:  ", self%lon

    SELECT CASE(self%plat)
    CASE (PLAT_BATHY)
       plat_type = "BATHY"
    CASE (PLAT_BUOY)
       plat_type = "BUOY"
    CASE (PLAT_FLOAT)
       plat_type = "FLOAT"
    CASE DEFAULT
       plat_type = "UNKNOWN"
    END SELECT
    PRINT *, "plat: ", TRIM(plat_type)

    DO i =1, SIZE(self%depth)
       v1 = MERGE(self%temp(i), PROF_UNDEF, SIZE(self%temp)>=i)
       v2 = MERGE(self%salt(i), PROF_UNDEF, SIZE(self%salt)>=i)
       PRINT '(I5, F8.2, F10.3, F10.3)', i, self%depth(i), v1, v2
    END DO

  END SUBROUTINE profile_print
  !=============================================================================


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
