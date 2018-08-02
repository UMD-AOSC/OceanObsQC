!===============================================================================
!> This is a template for a quality control step class. To use, copy to a new
!> file, replace all instances of "QCTEMPLATE" with a unique name of your class,
!> and add an entry to the "CMakeLists.txt" file in this directory under the
!> "SET(PLUGINS" line.
!> In order forthe automatic plugin loader to work, the following rules
!> must be followed:
!>  * class name is  <QCTEMPLATE>
!>  * filename is    <QCTEMPLATE>.F90
!>  * module name is <QCTEMPLATE>_mod
!-------------------------------------------------------------------------------
MODULE QCTEMPLATE_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: QCTEMPLATE
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE         :: check => qc_step_check
  END TYPE QCTEMPLATE
  !=============================================================================


CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this qc step plugin
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "QCTEMPLATE"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short description of what this QC step does
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "This is a blank QC step template... it doesn't do anything"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform the quality control on the input observations
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_check(self, obs_in, obs_out)
    CLASS(QCTEMPLATE), INTENT(inout) :: self
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out

    INTEGER :: i
    TYPE(profile) :: prof_in, prof_out


    DO i = 1, obs_in%SIZE()
       prof_in = obs_in%get(i)

       ! This is where the special checks would be done
       ! For now, take the profile the way it is.
       prof_out = prof_in

       CALL obs_out%push_back(prof_out)
    END DO


  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE QCTEMPLATE_mod
