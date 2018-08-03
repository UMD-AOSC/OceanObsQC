MODULE qc_depths_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_depths

     INTEGER :: min_depths = 3
     REAL    :: max_depth = 10000

   CONTAINS

     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE         :: check => qc_step_check
  END TYPE qc_depths
  !=============================================================================


CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this qc step plugin
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_depths"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short description of what this QC step does
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "checks for valid depth levels"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform the quality control on the input observations
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_check(self, obs_in, obs_out)
    CLASS(qc_depths), INTENT(inout) :: self
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
       IF(SIZE(prof%depth) < self%min_depths) THEN
          bad_minpoints = bad_minpoints + 1
          CYCLE
       END IF

       ! check unrealistic max depth
       IF(MAXVAL(prof%depth) > self%max_depth) THEN
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
