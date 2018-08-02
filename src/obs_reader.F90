MODULE obs_reader_mod
  USE profile_mod
  use ftlDynArrayProfileModule

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
       IMPORT obs_reader, ftlDynArrayProfile
       CLASS(obs_reader) :: self
       TYPE(ftlDynArrayProfile), INTENT(inout) :: obs
     END SUBROUTINE I_reader_read
  END INTERFACE
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: obs_reader_ptr
     CLASS(obs_reader), POINTER :: p
  END TYPE obs_reader_ptr
  !=============================================================================


  ! private variables to handle registration of reader plugins
  !-----------------------------------------------------------------------------
  INTEGER, PARAMETER   :: plugin_reg_max = 100
  INTEGER              :: plugin_reg_num = 0
  TYPE(obs_reader_ptr) :: plugin_reg(plugin_reg_max)

!  type(ftlDynArrayobs_reader_ptr) :: foo

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


  SUBROUTINE obs_reader_init(obs)
    TYPE(ftlDynArrayProfile), intent(out) :: obs
    integer :: i

    
    PRINT *, plugin_reg(1)%p%name()
    
    call obs%new()

    ! read in profiles from the desired plugin
    ! TODO, specify plugin with namelist
    CALL plugin_reg(1)%p%obs_read(obs)

    
    print *, "Total obs loaded: ", obs%size()

  END SUBROUTINE obs_reader_init

END MODULE obs_reader_mod


! #define FTL_TEMPLATE_TYPE obs_reader_ptr
! #define FTL_TEMPLATE_TYPE_IS_DERIVED
! #define FTL_TEMPLATE_TYPE_NAME obsReaderPtr
! #define FTL_TEMPLATE_TYPE_MODULE obs_reader_mod
! #define FTL_INSTANTIATE_TEMPLATE
! #include "ftlDynArray.F90_template"
