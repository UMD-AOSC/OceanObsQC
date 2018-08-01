MODULE obs_writer_nc_mod
  USE obs_writer_mod

  IMPLICIT NONE
  PRIVATE



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_writer), PUBLIC :: obs_writer_nc
   CONTAINS
     PROCEDURE, NOPASS :: name => obs_writer_nc_get_name
  END TYPE obs_writer_nc
  !=============================================================================



CONTAINS



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION obs_writer_nc_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "NC"
  END FUNCTION obs_writer_nc_get_name
  !=============================================================================


END MODULE obs_writer_nc_mod
