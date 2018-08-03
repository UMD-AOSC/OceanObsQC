MODULE obs_reader_nc_mod
  USE obs_reader_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_reader), PUBLIC :: obs_reader_nc
   CONTAINS
     PROCEDURE, NOPASS :: name => reader_nc_get_name
     PROCEDURE         :: obs_read => reader_nc_read
  END TYPE obs_reader_nc
  !=============================================================================



CONTAINS



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION reader_nc_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "NC"
  END FUNCTION reader_nc_get_name
  !=============================================================================

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE reader_nc_read(self, filename, obs)
    CLASS(obs_reader_nc) :: self
    CHARACTER(len=*),  INTENT(in)    :: filename
    TYPE(vec_profile), INTENT(inout) :: obs

    PRINT *, "NetCDF reader not yet implemented"
    STOP 1
  END SUBROUTINE reader_nc_read
  !=============================================================================

END MODULE obs_reader_nc_mod
