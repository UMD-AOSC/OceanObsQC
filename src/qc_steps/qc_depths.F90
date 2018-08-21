!===============================================================================
!> QC step to filter out observations with bad depth values.
!! Removes a profile if any of these are true:
!!  1) not enough vertical levels
!!  2) Staring depth is too deep
!!  3) max depth is unrealistic
!!  4) depths do not increase monotonically
!!  5) there is a large vertical gap
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


  ! parameters to be read in from the namelist
  LOGICAL :: check_nonmono = .TRUE.
  INTEGER :: min_levels = 3
  REAL    :: max_depth = 10000
  REAL    :: max_start = 10.0
  REAL    :: max_gap   = 500.0



CONTAINS

  !==> 2018-08-20 :: in obs_in has platform type  (bathy, tesac, float)

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

    NAMELIST /qc_depths/ check_nonmono, min_levels, max_start, max_depth, max_gap
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
  SUBROUTINE qc_step_check(obs_in, obs_out, obs_rej)
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    INTEGER :: i, j
    TYPE(profile) :: prof

    INTEGER :: bad_minpoints, bad_maxdepth, bad_nonmono, bad_deepstart, &
         bad_largegap

    bad_minpoints = 0
    bad_maxdepth = 0
    bad_nonmono = 0
    bad_deepstart = 0
    bad_largegap  = 0

    ! check each profile
    each_profile: DO i = 1, obs_in%SIZE()
       prof = obs_in%get(i)


       ! remove if profile does not have enough levels
       IF(min_levels > 0 .AND. SIZE(prof%depth) < min_levels) THEN
          bad_minpoints = bad_minpoints + 1
          prof%hour=31.0
          CALL obs_rej%push_back(prof)          
          CYCLE
       END IF


       ! check first level too deep
       IF(max_start > 0 .AND. prof%depth(1) > max_start) THEN
          bad_deepstart = bad_deepstart + 1
          prof%hour=32.0
          CALL obs_rej%push_back(prof)          
          CYCLE
       END IF


       ! check unrealistic max depth
       IF(max_depth > 0 .AND. MAXVAL(prof%depth) > max_depth) THEN
          bad_maxdepth = bad_maxdepth + 1
          prof%hour=33.0
          CALL obs_rej%push_back(prof)          
          CYCLE
       END IF


       ! loop for several tests that compare each level to the next
       each_lvl: DO j=2,SIZE(prof%depth)

          ! check for non-monotonic depths
          IF (check_nonmono .AND. prof%depth(j) <= prof%depth(j-1)) THEN
             bad_nonmono = bad_nonmono + 1
             prof%hour=34.0
             CALL obs_rej%push_back(prof)          
             CYCLE each_profile
          END IF

          ! check for large vertical gap
          IF (max_gap > 0 .AND. prof%depth(j) - prof%depth(j-1) > max_gap) THEN
             bad_largegap = bad_largegap + 1
             prof%hour=35.0
             CALL obs_rej%push_back(prof)          
             CYCLE each_profile
          END IF
       END DO each_lvl


       ! profile passes checks, add it to the output list
       CALL obs_out%push_back(prof)

    END DO each_profile


    ! print out stats if any profiles were removed
    ! NOTE: these should be printed in the order that they were checked
    IF(bad_minpoints > 0)&
         PRINT '(I8,A)', bad_minpoints, ' profiles removed for too few levels, h31'
    IF(bad_deepstart > 0)&
         PRINT '(I8,A)', bad_deepstart, ' profiles removed for starting too deep, h32.'
    IF(bad_maxdepth > 0)&
         PRINT '(I8,A)', bad_maxdepth, ' profiles removed for max depth too large, h33'
    IF(bad_nonmono > 0)&
         PRINT '(I8,A)', bad_nonmono, ' profiles removed for non-monotonic depths, h34'
    IF(bad_largegap > 0)&
         PRINT '(I8,A)', bad_largegap, ' profiles removed for large vertical gap.  h35'

  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_depths_mod
