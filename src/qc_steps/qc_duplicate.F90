!===============================================================================
!> Remove duplicate profiles.
!! profiles must have the same number of levels, identifier, date
!! and lat, lon, hour values that are within eror of being the same
!-------------------------------------------------------------------------------
MODULE qc_duplicate_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_duplicate
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE, NOPASS :: init  => qc_step_init
     PROCEDURE, NOPASS :: check => qc_step_check
  END TYPE qc_duplicate
  !=============================================================================


CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_duplicate"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "remove duplicate profiles"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only called once, even if the qc_step_check
  !! subroutine is called multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(nmlfile)
    INTEGER, INTENT(in) :: nmlfile

    !NAMELIST /qc_duplicate/ var1, var2
    !READ(nmlfile, qc_duplicate)
    !PRINT qc_duplicate

  END SUBROUTINE qc_step_init
  !=============================================================================



  !=============================================================================
  !> Perform the quality control on the input observations.
  !!  Each profile in "prof_in" should be checked, and if valid added to
  !!  "prof_out". Profiles can be combined, removed, added, left alone...
  !!  The number of profiles in "prof_out" does not need to be the same as
  !!  "prof_in".
  !! @param obs_in   a vector of input "profile" types
  !! @param obs_out  a vector of the output "profile" types
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_check(obs_in, obs_out)
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out

    INTEGER :: i, j
    TYPE(profile), POINTER :: prof1, prof2

    INTEGER :: count

    count = 0

    outer: DO i = 1, obs_in%SIZE()
       prof1 => obs_in%of(i)

       inner: DO j = i+1, obs_in%SIZE()
          prof2 => obs_in%of(j)

          IF( prof1%id /= prof2%id ) CYCLE inner
          IF( ABS(prof1%lat - prof2%lat) > 0.001) CYCLE inner
          IF( ABS(prof1%lon - prof2%lon) > 0.001) CYCLE inner
          IF( prof1%date /= prof2%date) CYCLE inner
          IF( ABS(prof1%hour - prof2%hour) > 0.01) CYCLE inner
          IF( prof1%plat /= prof1%plat) CYCLE inner
          IF( SIZE(prof1%depth) /= SIZE(prof2%depth) ) CYCLE inner
          IF( SIZE(prof1%temp) /= SIZE(prof2%temp) ) CYCLE inner
          IF( SIZE(prof1%salt) /= SIZE(prof2%salt) ) CYCLE inner

          ! if we made it this far, then prof1 and prof2 are identical
          ! skip to the next profile to check
          count = count + 1
          CYCLE outer
       END DO inner

       CALL obs_out%push_back(prof1)
    END DO outer

    IF(count > 0)&
         PRINT '(I8,A)', count, ' duplicate profiles removed'

  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_duplicate_mod
