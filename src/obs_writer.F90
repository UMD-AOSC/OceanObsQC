MODULE obs_writer_mod
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: obs_writer_register


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: obs_writer
   CONTAINS
     PROCEDURE(I_obs_writer_getstr), NOPASS, DEFERRED :: name
  END TYPE obs_writer

  ABSTRACT INTERFACE
     FUNCTION I_obs_writer_getstr()
       CHARACTER(:), ALLOCATABLE :: I_obs_writer_getstr
     END FUNCTION I_obs_writer_getstr
  END INTERFACE
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE :: obs_writer_ptr
     CLASS(obs_writer), POINTER :: p
  END TYPE obs_writer_ptr
  !=============================================================================


  ! private variables to handle registration of writer plugins
  !-----------------------------------------------------------------------------
  INTEGER, PARAMETER   :: plugin_reg_max = 100
  INTEGER              :: plugin_reg_num = 0
  TYPE(obs_writer_ptr) :: plugin_reg(plugin_reg_max)



CONTAINS


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE obs_writer_register(plugin)
    CLASS(obs_writer), POINTER :: plugin
    INTEGER :: i

    ! make sure we haven't reached our max number of plugins
    IF (plugin_reg_num == plugin_reg_max) THEN
       PRINT *, "ERROR: too many obs_writer plugins registered."
       STOP 1
    END IF

    ! make sure a plugin of this name hasn't already been registered
    !  do i=1,

    ! add the plugin to the list
    plugin_reg_num = plugin_reg_num + 1
    plugin_reg(plugin_reg_num)%p => plugin

    PRINT *, "* ", plugin%name()
  END SUBROUTINE obs_writer_register
  !=============================================================================



END MODULE obs_writer_mod
