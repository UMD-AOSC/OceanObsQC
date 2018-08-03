MODULE obs_reader_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE


  !=============================================================================
  !> Abstract class from which all observation readers are based.
  !> Each class derrived from this must have a unique "name"
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: obs_reader
   CONTAINS
     PROCEDURE(I_reader_getstr), NOPASS, DEFERRED :: name
     PROCEDURE(I_reader_read),           DEFERRED :: obs_read
  END TYPE obs_reader

  ABSTRACT INTERFACE
     FUNCTION I_reader_getstr()
       CHARACTER(:), ALLOCATABLE :: I_reader_getstr
     END FUNCTION I_reader_getstr

     SUBROUTINE I_reader_read(self, filename, obs)
       IMPORT obs_reader, vec_profile
       CLASS(obs_reader) :: self
       CHARACTER(len=*),  INTENT(in)    :: filename
       TYPE(vec_profile), INTENT(inout) :: obs
     END SUBROUTINE I_reader_read
  END INTERFACE
  !=============================================================================


  !=============================================================================
  !> Simple wrapper for the obs_reader type so that it can be placed
  !> in a vector object
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: obs_reader_ptr
     CLASS(obs_reader), POINTER :: p
  END TYPE obs_reader_ptr
  !=============================================================================

END MODULE obs_reader_mod
!===============================================================================




!===============================================================================
!> Templated vector object for holding obs_reader_ptr types
!-------------------------------------------------------------------------------
MODULE vec_obs_reader_mod
  USE obs_reader_mod
#define _type type(obs_reader_ptr)
#define _vector vec_obs_reader
#include "templates/vector.inc"
END MODULE vec_obs_reader_mod
!===============================================================================
