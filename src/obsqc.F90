PROGRAM obsqc
  ! obs_reader plugins
  USE obs_reader_plugins
  USE vec_obs_reader_mod
  USE obs_reader_mod

  ! obs_writer plugins
  USE obs_writer_plugins
  USE vec_obs_writer_mod
  USE obs_writer_mod

  ! type to hold profiles
  USE profile_mod
  USE vec_profile_mod


  IMPLICIT NONE


  INTEGER :: i
  TYPE(vec_profile) :: obs
  TYPE(profile) :: ob

  TYPE(obs_reader_ptr) :: reader_ptr
  TYPE(obs_writer_ptr) :: writer_ptr


  PRINT *, "Ocean Observation Qualtiy Control"


  ! Determine which plugin to use for reading observations
  PRINT *, ""
  PRINT *, "Available obs reader plugins:"
  CALL register_obs_reader_plugins()
  DO i = 1, obs_readers%SIZE()
     reader_ptr = obs_readers%get(i)
     PRINT *, "* ",reader_ptr%p%name()
  END DO


  ! Determine which plugin to use for writing observations at the end
  PRINT *, ""
  PRINT *, "Available obs writer plugins:"
  CALL register_obs_writer_plugins()
  DO i = 1, obs_writers%SIZE()
     writer_ptr = obs_writers%get(i)
     PRINT *, "* ",writer_ptr%p%name()
  END DO


END PROGRAM obsqc
