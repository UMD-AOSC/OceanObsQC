MODULE obs_writer_nc_mod
  USE obs_writer_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_writer), PUBLIC :: obs_writer_nc
   CONTAINS
     PROCEDURE, NOPASS :: name => writer_nc_get_name
     PROCEDURE         :: obs_write => writer_nc_write
  END TYPE obs_writer_nc
  !=============================================================================



CONTAINS



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION writer_nc_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "NC"
  END FUNCTION writer_nc_get_name
  !=============================================================================


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE writer_nc_write(self, obs)
    CLASS(obs_writer_nc), INTENT(inout) :: self
    TYPE(vec_profile), INTENT(in) :: obs

  END SUBROUTINE writer_nc_write
  !=============================================================================

END MODULE obs_writer_nc_mod
