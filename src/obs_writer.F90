MODULE obs_writer_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: obs_writer
   CONTAINS
     PROCEDURE(I_writer_getstr), NOPASS, DEFERRED :: name
     PROCEDURE(I_writer_write),          DEFERRED :: obs_write
  END TYPE obs_writer

  ABSTRACT INTERFACE
     FUNCTION I_writer_getstr()
       CHARACTER(:), ALLOCATABLE :: I_writer_getstr
     END FUNCTION I_writer_getstr

     SUBROUTINE I_writer_write(self, obs)
       IMPORT obs_writer, vec_profile
       CLASS(obs_writer), INTENT(inout) :: self
       TYPE(vec_profile), INTENT(in) :: obs
     END SUBROUTINE I_writer_write

  END INTERFACE
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: obs_writer_ptr
     CLASS(obs_writer), POINTER :: p
  END TYPE obs_writer_ptr
  !=============================================================================


END MODULE obs_writer_mod



!===============================================================================
!===============================================================================



MODULE vec_obs_writer_mod
  USE obs_writer_mod
#define _type type(obs_writer_ptr)
#define _vector vec_obs_writer
#include "templates/vector.inc"
END MODULE vec_obs_writer_mod
