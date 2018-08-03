MODULE obs_reader_wod_mod
  USE obs_reader_mod
  USE profile_mod
  USE vec_profile_mod

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
  SUBROUTINE wod_read(self, filename, obs)
    CLASS(obs_reader_wod) :: self
    CHARACTER(len=*),  INTENT(in)    :: filename
    TYPE(vec_profile), INTENT(inout) :: obs

    PRINT *, "WOD reader not yet implemented"
    STOP 1
  END SUBROUTINE wod_read
  !=============================================================================


END MODULE obs_reader_wod_mod
