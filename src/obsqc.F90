PROGRAM obsqc

  ! obs_reader plugins
  USE obs_reader_plugins
  USE vec_obs_reader_mod
  USE obs_reader_mod

  ! obs_writer plugins
  USE obs_writer_plugins
  USE vec_obs_writer_mod
  USE obs_writer_mod

  ! quality control step plugins
  USE qc_step_plugins
  USE vec_qc_step_mod
  USE qc_step_mod

  ! type to hold profiles
  USE profile_mod
  USE vec_profile_mod


  IMPLICIT NONE

  INTEGER :: i
  TYPE(vec_profile) :: obs, obs2
  TYPE(profile) :: ob

  TYPE(obs_reader_ptr) :: obs_reader_wrapper
  TYPE(obs_writer_ptr) :: obs_writer_wrapper
  TYPE(qc_step_ptr)    :: qc_step_wrapper

  CLASS(obs_reader), POINTER :: selected_obs_reader
  CLASS(obs_writer), POINTER :: selected_obs_writer

  PRINT *, ""
  PRINT *, "==================================================================="
  PRINT *, "NCEP Ocean Observation Qualtiy Control"
  PRINT *, " Git repo version: ", CVERSION
  PRINT *, "==================================================================="


  ! Determine which plugin to use for reading observations
  PRINT *, ""
  PRINT *, "Available observation reader plugins:"
  CALL register_obs_reader_plugins()
  selected_obs_reader=>NULL()
  DO i = 1, obs_readers%SIZE()
     obs_reader_wrapper = obs_readers%get(i)
     PRINT *, "* ", obs_reader_wrapper%p%name()
     ! TODO, add the real selection here
     IF (i == 1) selected_obs_reader => obs_reader_wrapper%p
  END DO

  ! Determine which plugin to use for writing observations at the end
  PRINT *, ""
  PRINT *, "Available observation writer plugins:"
  CALL register_obs_writer_plugins()
  DO i = 1, obs_writers%SIZE()
     obs_writer_wrapper = obs_writers%get(i)
     PRINT *, "* ", obs_writer_wrapper%p%name()
     ! TODO, add the real selection where
     IF (i==1) selected_obs_writer => obs_writer_wrapper%p
  END DO

  PRINT *, ""
  !  PRINT *, "Available QC step plugins:"
  CALL register_qc_step_plugins()
  !DO i = 1, qc_steps%SIZE()
  !qc_step_wrapper = qc_steps%get(i)
  !PRINT *, "* ", qc_step_wrapper%p%name(), ' - (',qc_step_wrapper%p%desc(),')'
  !END DO



  ! read in the observations
  PRINT *, ""
  PRINT *, "Reading profiles"
  PRINT *, "---------------------------------------------"
  CALL selected_obs_reader%obs_read(obs)
  PRINT '(A,I0,A)', " Read ", obs%SIZE(), " profiles."
  PRINT *, ""

  ! call each qc step on it
  DO i=1, qc_steps%SIZE()
     obs2 = vec_profile()

     ! perform the QC step
     qc_step_wrapper = qc_steps%get(i)
     PRINT *, ""
     PRINT *, qc_step_wrapper%p%name(), ' - (',qc_step_wrapper%p%desc(),')'
     PRINT *, "---------------------------------------------"
     CALL qc_step_wrapper%p%check(obs, obs2)

     ! check to see how many, if any, profiles were removed
     !IF(obs%SIZE() /= obs2%SIZE()) THEN
     !PRINT *, qc_step_wrapper%p%name(), " - removed ", obs%SIZE() - obs2%SIZE()
     !END IF

     ! get ready for next cycle
     obs = obs2
  END DO

  ! write out the obserations
  PRINT *, ""
  PRINT *, "Writing profiles"
  PRINT *, "---------------------------------------------"
  PRINT '(A,I0,A)', " Writing ", obs%SIZE(), " profiles."
  CALL selected_obs_writer%obs_write(obs)

END PROGRAM obsqc
