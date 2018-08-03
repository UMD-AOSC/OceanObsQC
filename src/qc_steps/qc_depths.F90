!===============================================================================
!> QC plugin to filter out observations with bad depth values.
!! Removes a profile if any of these are true:
!!  1) not enough vertical levels
!!  2) max depth is unrealistic
!!  3) depths do not increase monotonically
!-------------------------------------------------------------------------------
MODULE qc_depths_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_depths
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE, NOPASS :: init  => qc_step_init
     PROCEDURE, NOPASS :: check => qc_step_check
  END TYPE qc_depths
  !=============================================================================

  INTEGER :: min_levels = 3
  REAL    :: max_depth = 10000

CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_depths"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "checks for valid depth levels"
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

    NAMELIST /qc_depths/ min_levels, max_depth
    READ(nmlfile, qc_depths)
    PRINT qc_depths

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
    TYPE(profile) :: prof
    LOGICAL :: valid

    INTEGER :: bad_minpoints, bad_maxdepth, bad_nonmono

    bad_minpoints = 0
    bad_maxdepth = 0
    bad_nonmono = 0

    ! check each profile
    DO i = 1, obs_in%SIZE()
       prof = obs_in%get(i)

       ! remove if profile does not have enough levels
       IF(SIZE(prof%depth) < min_levels) THEN
          bad_minpoints = bad_minpoints + 1
          CYCLE
       END IF

       ! check unrealistic max depth
       IF(MAXVAL(prof%depth) > max_depth) THEN
          bad_maxdepth = bad_maxdepth + 1
          CYCLE
       END IF

       ! check for non-monotonic depths
       valid = .TRUE.
       DO j=2,SIZE(prof%depth)
          IF (prof%depth(j) < prof%depth(j-1)) THEN
             bad_nonmono = bad_nonmono + 1
             valid = .FALSE.
             EXIT
          END IF
       END DO
       IF(.NOT. valid) CYCLE

       ! profile passes checks, add it to the output list
       CALL obs_out%push_back(prof)

    END DO


    ! print out stats if any profiles were removed
    IF(bad_minpoints > 0)&
         PRINT '(I8,A)', bad_minpoints, ' profiles removed for too few levels'
    IF(bad_maxdepth > 0)&
         PRINT '(I8,A)', bad_maxdepth, ' profiles removed for max depth too large'
    IF(bad_nonmono > 0)&
         PRINT '(I8,A)', bad_nonmono, ' profiles removed for non-monotonic depths'

  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_depths_mod
