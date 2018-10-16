MODULE qc_step_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !> Abstract  class from which all QC steps are based.
  !> Each class derrived from this must have a unique "name"
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: qc_step
   CONTAINS
     PROCEDURE(I_qc_step_getstr), NOPASS, DEFERRED :: name
     PROCEDURE(I_qc_step_getstr), NOPASS, DEFERRED :: desc
     PROCEDURE(I_qc_step_init),   NOPASS, DEFERRED :: init
     PROCEDURE(I_qc_step_check),  NOPASS, DEFERRED :: check
  END TYPE qc_step

  ABSTRACT INTERFACE

     FUNCTION I_qc_step_getstr()
       CHARACTER(:), ALLOCATABLE :: I_qc_step_getstr
     END FUNCTION I_qc_step_getstr

     SUBROUTINE I_qc_step_init(nmlfile)
       INTEGER, INTENT(in) :: nmlfile
     END SUBROUTINE I_qc_step_init

     SUBROUTINE I_qc_step_check(obs_in, obs_out, obs_rej)
       IMPORT vec_profile
       TYPE(vec_profile), INTENT(in)    :: obs_in
       TYPE(vec_profile), INTENT(inout) :: obs_out
       TYPE(vec_profile), INTENT(inout) :: obs_rej
     END SUBROUTINE I_qc_step_check

  END INTERFACE
  !=============================================================================


  !=============================================================================
  !> Simple wrapper for the qc_step type so that it can be placed
  !> in a vector object
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: qc_step_ptr
     CLASS(qc_step), POINTER :: p
  END TYPE qc_step_ptr
  !=============================================================================

END MODULE qc_step_mod
!===============================================================================



!===============================================================================

!> Templated vector object for holding qc_step_ptr types
!-------------------------------------------------------------------------------
MODULE vec_qc_step_mod
  USE qc_step_mod
#define _type type(qc_step_ptr)
#define _vector vec_qc_step
#include "templates/vector.inc"
END MODULE vec_qc_step_mod
!===============================================================================
