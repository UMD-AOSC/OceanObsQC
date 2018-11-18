MODULE qc_step_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: print_rej_count


  !=============================================================================
  !> Abstract  class from which all QC steps are based.
  !> Each class derrived from this must have a unique "name"
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: qc_step
     INTEGER :: err_base
     LOGICAL :: initialized = .FALSE.
   CONTAINS
     PROCEDURE(I_qc_step_getstr), NOPASS, DEFERRED :: name
     PROCEDURE(I_qc_step_getstr), NOPASS, DEFERRED :: desc
     PROCEDURE(I_qc_step_init),   DEFERRED :: init
     PROCEDURE(I_qc_step_check),  DEFERRED :: check
  END TYPE qc_step

  ABSTRACT INTERFACE

     FUNCTION I_qc_step_getstr()
       CHARACTER(:), ALLOCATABLE :: I_qc_step_getstr
     END FUNCTION I_qc_step_getstr

     SUBROUTINE I_qc_step_init(self, nmlfile)
       IMPORT qc_step
       CLASS(qc_step) :: self
       INTEGER, INTENT(in) :: nmlfile
     END SUBROUTINE I_qc_step_init

     SUBROUTINE I_qc_step_check(self, obs_in, obs_out, obs_rej)
       IMPORT vec_profile
       IMPORT qc_step
       CLASS(qc_step) :: self
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


CONTAINS

  SUBROUTINE print_rej_count(count, desc, err_code)
    INTEGER, INTENT(IN) :: count
    CHARACTER(*), INTENT(IN) :: desc
    INTEGER, INTENT(IN) :: err_code

    IF(count > 0)&
         PRINT '(I8,A,I5,A)', count, ' '//desc//' (tag:',err_code,')'
  END SUBROUTINE

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
