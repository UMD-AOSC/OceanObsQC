MODULE obs_reader_mod
  USE profile_mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: obs_reader_register
  PUBLIC :: obs_reader_init


  !=============================================================================
  !>
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

     SUBROUTINE I_reader_read(self, obs)
       IMPORT obs_reader, profile
       CLASS(obs_reader) :: self
       TYPE(profile), ALLOCATABLE, INTENT(out) :: obs(:)
     END SUBROUTINE I_reader_read
  END INTERFACE
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE :: obs_reader_ptr
     CLASS(obs_reader), POINTER :: p
  END TYPE obs_reader_ptr
  !=============================================================================


  ! private variables to handle registration of reader plugins
  !-----------------------------------------------------------------------------
  INTEGER, PARAMETER   :: plugin_reg_max = 100
  INTEGER              :: plugin_reg_num = 0
  TYPE(obs_reader_ptr) :: plugin_reg(plugin_reg_max)



CONTAINS


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE obs_reader_register(plugin)
    CLASS(obs_reader), POINTER :: plugin
    INTEGER :: i

    ! make sure we haven't reached our max number of plugins
    IF (plugin_reg_num == plugin_reg_max) THEN
       PRINT *, "ERROR: too many obs_reader plugins registered."
       STOP 1
    END IF

    ! make sure a plugin of this name hasn't already been registered
    !  do i=1,

    ! add the plugin to the list
    plugin_reg_num = plugin_reg_num + 1
    plugin_reg(plugin_reg_num)%p => plugin

    PRINT *, "* ", plugin%name()
  END SUBROUTINE obs_reader_register
  !=============================================================================


  SUBROUTINE obs_reader_init()
    TYPE(profile), ALLOCATABLE :: obs(:)
    PRINT *, plugin_reg(1)%p%name()
    CALL plugin_reg(1)%p%obs_read(obs)
  END SUBROUTINE obs_reader_init

END MODULE obs_reader_mod
