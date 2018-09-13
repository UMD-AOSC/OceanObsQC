!===============================================================================
!> NCEP Ocean Profile Observation Quality Control
!!
!! This program reads in a file of profile observations, does several
!! quality control steps on the set of profiles, and writes the result out
!! to a new file.
!-------------------------------------------------------------------------------
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


  CHARACTER(len=1024) :: in_filename
  CHARACTER(len=1024) :: out_filename
  CHARACTER(len=1024) :: rej_filename

  INTEGER :: i, nmlfile
!<<<<<< HEAD
  TYPE(vec_profile) :: obs, obs2, obs3
  TYPE(profile) :: ob
!======
! TYPE(vec_profile) :: obs, obs2
!>>>>>> 8900c20d386a5e59337491fa88ae1decb2fd371f

  TYPE(obs_reader_ptr) :: obs_reader_wrapper
  TYPE(obs_writer_ptr) :: obs_writer_wrapper
  TYPE(qc_step_ptr)    :: qc_step_wrapper

  CLASS(obs_reader), POINTER :: selected_obs_reader
  CLASS(obs_writer), POINTER :: selected_obs_writer

  ! variables that are read in from the namelist
  CHARACTER(:), ALLOCATABLE :: obs_reader_type
  CHARACTER(:), ALLOCATABLE :: obs_writer_type
  INTEGER :: read_start_date = 0
  INTEGER :: read_end_date   = 99999999

  NAMELIST /obsqc_nml/ obs_reader_type, obs_writer_type, &
       read_start_date, read_end_date

  PRINT *, ""
  PRINT *, "==================================================================="
  PRINT *, "NCEP Ocean Observation Qualtiy Control"
  PRINT *, " Git repo version: ", CVERSION
  PRINT *, "==================================================================="


  ! get the command line arguments for input output filenames
  i = command_argument_COUNT()
  IF (i /= 2) THEN
     PRINT *, 'ERROR: call with "obsqc <inputfile> <outputfile>"'
     STOP 1
  END IF
  CALL get_command_ARGUMENT(1, VALUE=in_filename)
  CALL get_command_ARGUMENT(2, VALUE=out_filename)

  rej_filename=trim('rej_') // trim(out_filename)

  ! open the namelist file
  ! grab the main parameters we need
  OPEN(newunit=nmlfile, file='obsqc.nml')
  ALLOCATE(CHARACTER(len=1024) :: obs_reader_type)
  ALLOCATE(CHARACTER(len=1024) :: obs_writer_type)
  READ(nmlfile, obsqc_nml)
  obs_reader_type = TRIM(obs_reader_type)
  obs_writer_type = TRIM(obs_writer_type)
  PRINT obsqc_nml


  ! print a list of all the obs reader plugins registered,
  ! and determine which plugin to use
  PRINT *, ""
  PRINT *, "Available observation reader plugins:"
  CALL register_obs_reader_plugins()
  selected_obs_reader=>NULL()
  DO i = 1, obs_readers%SIZE()
     obs_reader_wrapper = obs_readers%get(i)
     PRINT *, "* ", obs_reader_wrapper%p%name()
     IF (obs_reader_wrapper%p%name() == obs_reader_type)&
          selected_obs_reader => obs_reader_wrapper%p
  END DO
  IF (.NOT. ASSOCIATED(selected_obs_reader)) THEN
     PRINT *, 'ERROR: observation reader type "', obs_reader_type, &
          '" not found. Check the namelist.'
     STOP 1
  END IF
  PRINT *, "Using: ", selected_obs_reader%name()


  ! Print a list of all obs writer plugins registered,
  ! and determine which plugin to use
  PRINT *, ""
  PRINT *, "Available observation writer plugins:"
  CALL register_obs_writer_plugins()
  selected_obs_writer=>NULL()
  DO i = 1, obs_writers%SIZE()
     obs_writer_wrapper = obs_writers%get(i)
     PRINT *, "* ", obs_writer_wrapper%p%name()
     IF (obs_writer_wrapper%p%name() == obs_writer_type)&
          selected_obs_writer => obs_writer_wrapper%p
  END DO
  IF (.NOT. ASSOCIATED(selected_obs_writer)) THEN
     PRINT *, 'ERROR: observation writer type "', obs_writer_type, &
          '" not found. Check the namelist.'
     STOP 1
  END IF
  PRINT *, "Using: ", selected_obs_writer%name()


  ! register all the qc_step plugins
  PRINT *, ""
  CALL register_qc_step_plugins()
  PRINT  '(A,I0)',' QC plugins registered: ', qc_steps%SIZE()


  ! read in the observations
  PRINT *, ""
  PRINT *, ""
  PRINT *, "---------------------------------------------"
  PRINT *, "Reading profiles"
  PRINT *, "---------------------------------------------"
  CALL selected_obs_reader%get(TRIM(in_filename), &
       read_start_date, read_end_date, obs)
  CALL prof_stats(obs)
  PRINT *, ""
  PRINT *, ""


  ! call each qc step on the profile list
  PRINT *, "---------------------------------------------"
  PRINT *, "Running QC step plugins"
  PRINT *, "---------------------------------------------"
  DO i=1, qc_steps%SIZE()
     obs2 = vec_profile()
   ! obs3 = vec_profile()
     qc_step_wrapper = qc_steps%get(i)

     PRINT *, ""
     PRINT *, ""
     PRINT '(I3,5A)', i, ') ', qc_step_wrapper%p%name(), ' - (',qc_step_wrapper%p%desc(),')'
     PRINT *, "---------------------------------------------"

     ! initialize the module
     REWIND(nmlfile)
     CALL qc_step_wrapper%p%init(nmlfile)

     ! perform the QC step
     CALL qc_step_wrapper%p%check(obs, obs2, obs3)

     ! get ready for next cycle
     obs = obs2
  END DO


  ! write out the obserations
  PRINT *, ""
  PRINT *, ""
  PRINT *, "---------------------------------------------"
  PRINT *, "Writing profiles"
  PRINT *, "---------------------------------------------"
  CALL prof_stats(obs)
  CALL selected_obs_writer%obs_write(out_filename, obs)

  !-- write out rejected data
  call prof_stats(obs3)
  CALL selected_obs_writer%obs_write(rej_filename, obs3)


CONTAINS



  !============================================================
  SUBROUTINE prof_stats(profs)
    TYPE(vec_profile), INTENT(in) :: profs

    INTEGER :: i
    INTEGER :: prf_t_cnt, prf_s_cnt, obs_t_cnt, obs_s_cnt
    TYPE(profile), POINTER :: prf


    prf_t_cnt = 0
    prf_s_cnt = 0
    obs_t_cnt = 0
    obs_s_cnt = 0

    DO i=1,obs%SIZE()
       prf => obs%of(i)
       IF ( SIZE(prf%temp) > 0) THEN
          prf_t_cnt = prf_t_cnt + 1
          obs_t_cnt = obs_t_cnt + SIZE(prf%temp)
       END IF
       IF ( SIZE(prf%salt) > 0) THEN
          prf_s_cnt = prf_s_cnt + 1
          obs_s_cnt = obs_s_cnt + SIZE(prf%salt)
       END IF
    END DO

    PRINT *, ""
    PRINT '(I10, A)', prf_t_cnt, ' TEMP profiles'
    PRINT '(I10, A)', prf_s_cnt, ' SALT profiles'
    PRINT *, ""
    PRINT '(I10, A)', obs_t_cnt, ' total TEMP observations'
    PRINT '(I10, A)', obs_s_cnt, ' total SALT observations'

  END SUBROUTINE prof_stats
  !============================================================

END PROGRAM obsqc
