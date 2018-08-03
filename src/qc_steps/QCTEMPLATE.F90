!===============================================================================
!> This is a template for a quality control step class. To use,
!!  1) copy to a new file
!!  2) replace all instances of "QCTEMPLATE" with a unique name of your class
!!  3) add an entry to the "CMakeLists.txt" file in this directory under the
!!     "SET(PLUGINS" line.
!!  4) replace this comment block with a meaningful description of what the
!!     QC plugin is supposed to do.
!!
!! In order for the automatic plugin loader to work, the following rules
!! must be followed:
!!  * class name is  <QCTEMPLATE>
!!  * filename is    <QCTEMPLATE>.F90
!!  * module name is <QCTEMPLATE>_mod
!-------------------------------------------------------------------------------
MODULE QCTEMPLATE_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: QCTEMPLATE
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE, NOPASS :: init  => qc_step_init
     PROCEDURE, NOPASS :: check => qc_step_check
  END TYPE QCTEMPLATE
  !=============================================================================


CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "QCTEMPLATE"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "This is a blank QC step template... it doesn't do anything"
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

    !NAMELIST /QCTEMPLATE/ var1, var2
    !READ(nmlfile, QCTEMPLATE)
    !PRINT QCTEMPLATE

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
