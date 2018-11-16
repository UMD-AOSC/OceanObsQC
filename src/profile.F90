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
  INTEGER, PUBLIC, PARAMETER :: PLAT_TESAC   = 4 !< TESAC format data


  !> used in the profile%salt or profile%temp variables to indicate there is
  !!  no recorded value for that level
  REAL,    PUBLIC, PARAMETER :: PROF_UNDEF = 1.0e10


  INTEGER, PUBLIC, PARAMETER :: PROF_CHECK_GOOD    = 0
  INTEGER, PUBLIC, PARAMETER :: PROF_CHECK_NO_VARS = 1
  INTEGER, PUBLIC, PARAMETER :: PROF_CHECK_NO_T    = 2
  INTEGER, PUBLIC, PARAMETER :: PROF_CHECK_NO_S    = 3
  INTEGER, PUBLIC, PARAMETER :: PROF_CHECK_RMLVL   = 4

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
     INTEGER :: tag                  !< tag for output data (e.g. in obs_rej, rejected type)
     REAL, ALLOCATABLE :: depth(:)   !< depth of each level (m)
     REAL, ALLOCATABLE :: salt(:)    !< salinity of each level (PSU)
     REAL, ALLOCATABLE :: temp(:)    !< temperature (C)
   CONTAINS
     PROCEDURE :: PRINT => profile_print
     PROCEDURE :: check => profile_check
     PROCEDURE :: copy => profile_copy
     PROCEDURE :: clear => profile_clear
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
    CASE (PLAT_TESAC)
       plat_type = "TESAC"
    CASE DEFAULT
       plat_type = "UNKNOWN"
    END SELECT
    PRINT *, "plat: ", TRIM(plat_type)
    PRINT *, "tag:  ", self%tag

    DO i =1, SIZE(self%depth)
       v1 = MERGE(self%temp(i), PROF_UNDEF, SIZE(self%temp)>=i)
       v2 = MERGE(self%salt(i), PROF_UNDEF, SIZE(self%salt)>=i)
       PRINT '(I5, F8.2, F10.3, F10.3)', i, self%depth(i), v1, v2
    END DO

  END SUBROUTINE profile_print
  !=============================================================================


  !=============================================================================
  !> Checks for valid variables
  !! Removes levels with no valid variables
  !! Removes entire variable if all PROF_UNDEF
  !-----------------------------------------------------------------------------
  SUBROUTINE profile_check(self, code)
    CLASS(profile), INTENT(inout) :: self
    INTEGER, INTENT(out) :: code
    ! 0 = good, 1= no valid variables
    ! 2 = T removed, 3 = S removed
    ! 4 = levels removed

    INTEGER :: j, k
    LOGICAL :: vSalt, vTemp

    REAL, ALLOCATABLE :: tmp_r(:)

    code = PROF_CHECK_GOOD

    ! remove levels that have undefined depth, or no values
    j=0
    DO k=1,SIZE(self%depth)
       ! is the salinity valid at this leve?
       vSalt = SIZE(self%salt) >= k
       IF (vSalt) vSalt =  self%salt(k) < PROF_UNDEF

       ! is the temperature valid at this level?
       vTemp = SIZE(self%temp) >= k
       IF (vTemp) vTemp =  self%temp(k) < PROF_UNDEF

       ! if valid temp or salinity..AND valid depth
       IF ( (vSalt .OR. vTemp) .AND. self%depth(k) < PROF_UNDEF) THEN
          j = j + 1
          self%depth(j) = self%depth(k)
          IF (SIZE(self%salt) >= k) self%salt(j) = self%salt(k)
          IF (SIZE(self%temp) >= k) self%temp(j) = self%temp(k)
       END IF
    END DO

    IF (j /= SIZE(self%depth)) THEN
       ! levels were removed... adjust array length
       code = PROF_CHECK_RMLVL
       ALLOCATE(tmp_r(j))

       tmp_r = self%depth(1:j)
       DEALLOCATE(self%depth)
       ALLOCATE(self%depth(j))
       self%depth = tmp_r

       IF(SIZE(self%temp) > 0) THEN
          tmp_r = self%temp(1:j)
          DEALLOCATE(self%temp)
          ALLOCATE(self%temp(j))
          self%temp = tmp_r
       END IF

       IF(SIZE(self%salt) > 0) THEN
          tmp_r = self%salt(1:j)
          DEALLOCATE(self%salt)
          ALLOCATE(self%salt(j))
          self%salt = tmp_r
       END IF

       DEALLOCATE(tmp_r)
    END IF

    ! if no valid temperature, remove entire array
    IF(SIZE(self%temp) > 0) THEN
       IF(MAXVAL(self%temp) == PROF_UNDEF .AND. MINVAL(self%temp) == PROF_UNDEF) THEN
          DEALLOCATE(self%temp)
          ALLOCATE(self%temp(0))
          code = PROF_CHECK_NO_T
       END IF
    END IF

    ! if no valid salt, remove entire array
    IF(SIZE(self%salt) > 0) THEN
       IF(MAXVAL(self%salt) == PROF_UNDEF .AND. MINVAL(self%salt) == PROF_UNDEF) THEN
          DEALLOCATE(self%salt)
          ALLOCATE(self%salt(0))
          code = PROF_CHECK_NO_S
       END IF
    END IF


    ! if no salt AND no temp, don't use this profile
    IF(SIZE(self%salt) == 0 .AND. SIZE(self%temp) == 0) THEN
       code = PROF_CHECK_NO_VARS
    END IF
  END SUBROUTINE profile_check



  FUNCTION profile_copy(self, ts) RESULT(copy)
    CLASS(profile), INTENT(inout) :: self
    CHARACTER, OPTIONAL :: ts
    TYPE(profile) :: copy

    IF(PRESENT(ts)) THEN
       IF(ts /= 'T' .AND. ts /= 'S') THEN
          PRINT *, "ERROR: profile%copy() must be called empty, or with 'T' or 'S' "
          STOP 1
       END IF
    END IF

    copy%date = self%date
    copy%hour = self%hour
    copy%id = self%id
    copy%lat = self%lat
    copy%lon = self%lon
    copy%plat = self%plat
    ALLOCATE(copy%depth(SIZE(self%depth)))
    copy%depth = self%depth

    IF(PRESENT(ts) .AND. ts =='T') THEN
       ALLOCATE(copy%temp(SIZE(self%temp)))
       copy%temp = self%temp
    ELSE
       ALLOCATE(copy%temp(0))
    END IF

    IF(PRESENT(ts) .AND. ts == 'S') THEN
       ALLOCATE(copy%salt(SIZE(self%salt)))
       copy%salt = self%salt
    ELSE
       ALLOCATE(copy%salt(0))
    END IF

  END FUNCTION profile_copy




  SUBROUTINE profile_clear(self, ts)
    CLASS(profile), INTENT(inout) :: self
    CHARACTER :: ts

    IF(ts == 'T') THEN
      DEALLOCATE(self%temp)
      ALLOCATE(self%temp(0))
    ELSE IF(ts == 'S') THEN
      DEALLOCATE(self%salt)
      ALLOCATE(self%salt(0))
    ELSE
      PRINT *, "ERROR: profile%clear() must be called with 'T' or 'S' "
      STOP 1
    END IF
  END SUBROUTINE profile_clear

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
