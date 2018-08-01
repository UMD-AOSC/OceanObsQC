MODULE obs_reader_wod_mod
  USE obs_reader_mod
  USE profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_reader), PUBLIC :: obs_reader_wod
   CONTAINS
     PROCEDURE, NOPASS :: name => wod_get_name
     PROCEDURE         :: obs_read => wod_read
  END TYPE obs_reader_wod
  !=============================================================================


CONTAINS


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION wod_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "WOD"
  END FUNCTION wod_get_name
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE wod_read(self, obs)
    CLASS(obs_reader_wod) :: self
    TYPE(profile), ALLOCATABLE, INTENT(out) :: obs(:)
  END SUBROUTINE wod_read
  !=============================================================================


END MODULE obs_reader_wod_mod
