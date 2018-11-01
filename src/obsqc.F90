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

  INTEGER :: i, j, nmlfile, err
  REAL :: timer_start, timer_end
  TYPE(vec_profile) :: obs, obs_good, obs_rej
  TYPE(profile), POINTER :: prof
  INTEGER :: rm_count(6)
  INTEGER :: qcsteps_idx

  TYPE(obs_reader_ptr) :: obs_reader_wrapper
  TYPE(obs_writer_ptr) :: obs_writer_wrapper
  TYPE(qc_step_ptr)    :: qc_step_wrapper

  CLASS(obs_reader), POINTER :: selected_obs_reader
  CLASS(obs_writer), POINTER :: selected_obs_writer

  ! variables that are read in from the namelist
  CHARACTER(:), ALLOCATABLE :: obs_reader_type
  CHARACTER(:), ALLOCATABLE :: obs_writer_type
  CHARACTER(:), ALLOCATABLE :: qcsteps
  INTEGER :: read_start_date = 0
  INTEGER :: read_end_date   = 99999999

  NAMELIST /obsqc_nml/ obs_reader_type, obs_writer_type, &
       read_start_date, read_end_date, qcsteps

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

  rej_filename=TRIM('rej_') // TRIM(out_filename)

  ! open the namelist file
  ! grab the main parameters we need
  OPEN(newunit=nmlfile, file='obsqc.nml')
  obs_reader_type = REPEAT(' ', 1024)
  obs_writer_type = REPEAT(' ', 1024)
  qcsteps = REPEAT(' ', 1024)
  READ(nmlfile, obsqc_nml)
  obs_reader_type = TRIM(obs_reader_type)
  obs_writer_type = TRIM(obs_writer_type)
  qcsteps = TRIM(qcsteps)
  PRINT obsqc_nml


  PRINT *, ""
  PRINT *, ""
  PRINT *, "---------------------------------------------"
  PRINT *, "Reading profiles"
  PRINT *, "---------------------------------------------"

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
  PRINT *, ""

  ! read in the observations
  CALL cpu_TIME(timer_start)
  REWIND(nmlfile)
  CALL selected_obs_reader%init(nmlfile)
  CALL selected_obs_reader%get(TRIM(in_filename), &
       read_start_date, read_end_date, obs)
  PRINT *,""
  PRINT *, "Profiles read in:"
  CALL prof_stats(obs)
  CALL cpu_TIME(timer_end)
  PRINT *, ""
  PRINT '(5X,A,F5.1,A)', "elapsed time: ", timer_end-timer_start,'s'
  PRINT *, ""


  ! call each qc step on the profile list
  PRINT *, "---------------------------------------------"
  PRINT *, "Running QC step plugins"
  PRINT *, "---------------------------------------------"

  ! register all the qc_step plugins
  PRINT *, ""
  CALL register_qc_step_plugins()
  PRINT  '(A,I0)',' QC plugins registered: ', qc_steps%SIZE()
  DO i=1, qc_steps%SIZE()
    qc_step_wrapper = qc_steps%get(i)
    print *, "* ", qc_step_wrapper%p%name()
  END DO

  obs_rej = vec_profile()

  DO WHILE( LEN(qcsteps) > 0)
    ! loop over all the qc step names specified in the namelist string
    qcsteps_idx = SCAN(qcsteps, ';, ')
    IF(qcsteps_idx == 0) qcsteps_idx = LEN(qcsteps)+1

    DO i=1, qc_steps%SIZE()
      qc_step_wrapper = qc_steps%get(i)

      ! if this matches the current qc step we are trying to match, continue
      IF (trim(qcsteps(:qcsteps_idx-1)) /= qc_step_wrapper%p%name()) CYCLE

      CALL cpu_TIME(timer_start)
      obs_good = vec_profile()

      PRINT *, ""
      PRINT *, ""
      PRINT '(I3,5A)', i, ') ', qc_step_wrapper%p%name(), ' - (',qc_step_wrapper%p%desc(),')'
      PRINT *, "---------------------------------------------"

      ! initialize the module
      REWIND(nmlfile)
      CALL qc_step_wrapper%p%init(nmlfile)

      ! perform the QC step
      CALL qc_step_wrapper%p%check(obs, obs_good, obs_rej)

      ! check that the variables of each profile are good
      obs = vec_profile()
      rm_count = 0
      each_profile: DO j = 1, obs_good%SIZE()
        prof => obs_good%of(j)

          ! remove bad variables /levels
          CALL prof%check(err)

          SELECT CASE(err)
          CASE (PROF_CHECK_NO_VARS)
             ! don't add this profile to the valid obs
             rm_count(1) = rm_count(1) + 1
             CALL obs_rej%push_back(prof)
             CYCLE each_profile
          CASE (PROF_CHECK_NO_T)
             rm_count(2) = rm_count(2) + 1
          CASE (PROF_CHECK_NO_S)
             rm_count(3) = rm_count(3) + 1
  !        CASE (PROF_CHECK_RMLVL)
  !           rm_count(4) = rm_count(4) + 1
          END SELECT

          CALL obs%push_back(prof)
       END DO each_profile

       ! were any profiles modified?
       IF (SUM(rm_count) > 0 ) THEN
          PRINT *, ""
          PRINT '(A)', "WARNING: The following errors occurred because invalid profiles were"
          PRINT '(A)', "         returned from the qc step. These errors should not happen."
          PRINT '(A)', "         Check the validity of the code for this QC step."
       END IF
       IF (rm_count(1) > 0) &
            PRINT '(A,I8,A)', "WARNING: ", rm_count(1), " profiles removed due to no valid T or S values."
       IF (rm_count(2) > 0) &
            PRINT '(A,I8,A)', "WARNING: ", rm_count(2), " T profiles removed due to constant PROF_UNDEF profile."
       IF (rm_count(3) > 0) &
            PRINT '(A,I8,A)', "WARNING: ", rm_count(3), " S profiles removed due to constant PROF_UNDEF profile."
  !     IF (rm_count(4) > 0) &
  !          PRINT '(A,I8,A)', "WARNING: ", rm_count(4), " individual levels removed due to PROF_UNDEF values."

       ! get ready for next cycle
       CALL cpu_TIME(timer_end)
       PRINT *, ""
       PRINT '(5X,A,F5.1,A)', "elapsed time: ", timer_end-timer_start,'s'
       PRINT *, ""
       EXIT
    END DO

    IF(i > qc_steps%SIZE()) THEN
      PRINT *, "ERROR: qc step specified in namelist was not found: '", trim(qcsteps(:qcsteps_idx-1)),"'"
      STOP 1
    END IF
    qcsteps = qcsteps(qcsteps_idx+1:)
  END DO


  ! write out the obserations
  PRINT *, ""
  PRINT *, ""
  PRINT *, "---------------------------------------------"
  PRINT *, "Writing profiles"
  PRINT *, "---------------------------------------------"

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
  PRINT *, ""

  PRINT *, "Good profiles written:"
  CALL cpu_TIME(timer_start)
  CALL prof_stats(obs)
  CALL selected_obs_writer%obs_write(out_filename, obs)
  CALL cpu_TIME(timer_end)
  PRINT *, ""
  PRINT '(5X,A,F5.1,A)', "elapsed time: ", timer_end-timer_start,'s'
  PRINT *, ""

  PRINT *, "Rejected profiles written:"
  CALL cpu_TIME(timer_start)
  CALL prof_stats(obs_rej)
  CALL selected_obs_writer%obs_write(rej_filename, obs_rej)
  CALL cpu_TIME(timer_end)
  PRINT *, ""
  PRINT '(5X,A,F5.1,A)', "elapsed time: ", timer_end-timer_start,'s'
  PRINT *, ""



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

    DO i=1,profs%SIZE()
       prf => profs%of(i)
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
