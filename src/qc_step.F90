MODULE qc_step_mod

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: qc_step
     !CONTAINS
     !PROCEDURE(I_obs_writer_getstr), NOPASS, DEFERRED :: name
  END TYPE qc_step
  !
  ! ABSTRACT INTERFACE
  !    FUNCTION I_obs_writer_getstr()
  !      CHARACTER(:), ALLOCATABLE :: I_obs_writer_getstr
  !    END FUNCTION I_obs_writer_getstr
  ! END INTERFACE
  !=============================================================================
END MODULE qc_step_mod
