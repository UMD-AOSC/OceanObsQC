!===============================================================================
!> This is a template for a quality control step class. To use,
!!  1) copy to a new file
!!  2) replace all instances of "QCTEMPLATE" with a unique name of your class
!!  3) add an entry to the "CMakeLists.txt" file in this directory under the
!!     "SET(PLUGINS" line.
!!  4) replace this comment block with a meaningful description of what the
!!     QC plugin is supposed to do.
!!
!! In order for the automatic plugin loader to work, the following rules
!! must be followed:
!!  * class name is  <QCTEMPLATE>
!!  * filename is    <QCTEMPLATE>.F90
!!  * module name is <QCTEMPLATE>_mod
!-------------------------------------------------------------------------------
MODULE qc_profile_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_profile
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE, NOPASS :: init  => qc_step_init
     PROCEDURE, NOPASS :: check => qc_step_check
  END TYPE qc_profile
  !=============================================================================
  ! parameters to be from namelist
  LOGICAL, PUBLIC :: do_qc_profile = .TRUE.                  ! do this QC
  REAL, PUBLIC :: prf_trlatN = 60.0                          ! max latitude for constant profile
  REAL, PUBLIC :: prf_trlatS = -60.0                         ! min latitude for constant profile
  REAL, PUBLIC :: prf_trTmin = 0.05                          ! min Temp diff in profile
  REAL, PUBLIC :: prf_trSmin = 0.01                          ! min Salt diff in profile
  REAL, PUBLIC :: prf_T1spmax = 0.5                          ! max dT/dz at surface in profile
  REAL, PUBLIC :: prf_S1spmax = 0.1                          ! max dS/dz at surface in profile
  REAL, PUBLIC :: prf_Tbspmax = 0.5                          ! max dT/dz at bottom in profile
  REAL, PUBLIC :: prf_Sbspmax = 0.1                          ! max dS/dz at bottom in profile
  REAL, PUBLIC :: prf_Trsmx = 1.75                           ! max rms of dT/dx**2 in profile
  REAL, PUBLIC :: prf_Srsmx = 1.00                           ! max rms of dS/dx**2 in profile

CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_profile"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "This is a QC step for T and S profile data checking"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only called once, even if the qc_step_check
  !! subroutine is called multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(nmlfile)
    INTEGER, INTENT(in) :: nmlfile

    !NAMELIST /QCTEMPLATE/ var1, var2
    !READ(nmlfile, QCTEMPLATE)
    !PRINT QCTEMPLATE

    NAMELIST /qc_profile/ do_qc_profile, &
         prf_trlatN, prf_trlatS, prf_trTmin, prf_trSmin, &
         prf_T1spmax, prf_Tbspmax, prf_S1spmax, prf_Sbspmax, &
         prf_Trsmx, prf_Srsmx

    !---- read namelist from qc_profile
    READ(nmlfile, qc_profile)
    PRINT qc_profile
    !----

  END SUBROUTINE qc_step_init
  !=============================================================================



  !=============================================================================
  !> Perform the quality control on the input observations.
  !!  Each profile in "prof_in" should be checked, and if valid added to
  !!  "prof_out". Profiles can be combined, removed, added, left alone...
  !!  The number of profiles in "prof_out" does not need to be the same as
  !!  "prof_in".
  !! @param obs_in   a vector of input "profile" types
  !! @param obs_out  a vector of the output "profile" types
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_check(obs_in, obs_out, obs_rej)
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    INTEGER :: i, k, kb, kn
    REAL :: trTmin, trTmax, trSmin, trSmax, dtdz, dsdz, dvns, dvrs
    TYPE(profile),POINTER :: prof
    INTEGER :: bad_prf_trconst_T, bad_prf_trconst_S, bad_prf_trconst
    INTEGER :: bad_prf_T1sp, bad_prf_Tbsp, bad_prf_S1sp, bad_prf_Sbsp
    INTEGER :: bad_prf_Tvns, bad_prf_Svns

    !---
    IF (.NOT. do_qc_profile) THEN
       PRINT *, "Skip qc_profile"
       obs_out = obs_in
       RETURN
    ENDIF

    !---
    bad_prf_trconst_T = 0
    bad_prf_trconst_S = 0
    bad_prf_trconst = 0
    bad_prf_T1sp = 0
    bad_prf_Tbsp = 0
    bad_prf_S1sp = 0
    bad_prf_Sbsp = 0
    bad_prf_Tvns = 0
    bad_prf_Svns = 0

    !--- main loop
    main : DO i = 1, obs_in%SIZE()
       prof => obs_in%of(i)

       !------------------------------------------------------------------------
       !--- $ check constant profile
       !------------------------------------------------------------------------
       IF (prof%lat <= prf_trlatN .AND. prof%lat >= prf_trlatS) THEN

          !---Temperature
          IF (SIZE(prof%temp) /= 0) THEN
             trTmin = prof%temp(1)
             trTmax = prof%temp(1)
             DO k = 2, SIZE(prof%temp)
                IF (trTmin > prof%temp(k)) trTmin = prof%temp(k)
                IF (trTmax < prof%temp(k)) trTmax = prof%temp(k)
             ENDDO
             IF (ABS(trTmax-trTmin) < prf_trTmin) THEN
                ! TODO, need to copy the profile before adding it to the
                ! reject vector, otherwise the actual profile values will
                ! not be retained (prof holds pointers to object, so the
                ! contents of prof are currently being modified after
                ! pushed back), same applies to salinity below.
                bad_prf_trconst_T = bad_prf_trconst_T + 1
                prof%tag = 60
                CALL obs_rej%push_back(prof)
                DEALLOCATE(prof%temp)
                ALLOCATE(prof%temp(0))
             END IF
          ENDIF ! (SIZE(prof%temp) /= 0)

          ! Salinity
          IF (SIZE(prof%salt) /= 0) THEN
             trSmin = prof%salt(1)
             trSmax = prof%salt(1)
             DO k = 2, SIZE(prof%salt)
                IF (trSmin > prof%salt(k)) trSmin = prof%salt(k)
                IF (trSmax < prof%salt(k)) trSmax = prof%salt(k)
             ENDDO
             IF (ABS(trSmax-trSmin) < prf_trSmin) THEN
                bad_prf_trconst_S = bad_prf_trconst_S + 1
                prof%tag = 61
                CALL obs_rej%push_back(prof)
                DEALLOCATE(prof%salt)
                ALLOCATE(prof%salt(0))
             ENDIF ! (SIZE(prof%salt) /= 0)
          ENDIF ! (trSmax-trSmin < prf_trSmin)

          IF(SIZE(prof%salt) == 0 .AND. SIZE(prof%temp) == 0) THEN
             !IF (tr_goodT + tr_goodS == 0) THEN
             bad_prf_trconst = bad_prf_trconst + 1
             prof%tag = 62
             CALL obs_rej%push_back(prof)
             CYCLE main
          ENDIF

       ENDIF ! (pronf%lat <= prf_trlatN .and. pronf%lat >= prf_trlatS)


       !------------------------------------------------------------------------
       !--- $ check spikiness in Temp
       !------------------------------------------------------------------------
       IF (SIZE(prof%temp) >= 2 .AND. prof%temp(1) /= PROF_UNDEF) THEN

          !-- dtdz at surface
          dtdz = (prof%temp(1)-prof%temp(2))/(prof%depth(2)-prof%depth(1))
          IF (ABS(dtdz) > prf_T1spmax) THEN
             !-dtdz, prof%temp(1),prof%temp(2),prof%temp(3)
             prof%temp(1) = prof%temp(2)
             bad_prf_T1sp = bad_prf_T1sp + 1
             prof%tag = 64
             CALL obs_rej%push_back(prof)
          ENDIF ! (ABS(dtdz) > prf_T1spmax)

          !-- dtdz at bottom
          kb = SIZE(prof%temp)
          dtdz = (prof%temp(kb-1)-prof%temp(kb))/(prof%depth(kb)-prof%depth(kb-1))
          IF (ABS(dtdz) > prf_Tbspmax) THEN
             !-dtdz, prof%temp(kb-2),prof%temp(kb-1),prof%temp(kb)
             bad_prf_Tbsp = bad_prf_Tbsp + 1
             prof%tag = 65
             CALL obs_rej%push_back(prof)
             prof%temp(kb) = PROF_UNDEF
          ENDIF ! (ABS(dtdz) > prf_Tbspmax)

          !-- noise check base on rms of dtdz**2
          dvns = 0.0
          kn = 0
          DO k = 2, SIZE(prof%temp)
             IF (prof%temp(k) /= PROF_UNDEF) THEN
                dtdz = (prof%temp(k-1)-prof%temp(k))/(prof%depth(k)-prof%depth(k-1))
                dvns = dvns + dtdz**2
                kn = kn + 1
             ENDIF
          ENDDO
          IF (kn == 0) kn = 1  ! just for check
          dvrs = SQRT(dvns/kn)
          IF (dvrs > prf_Trsmx) THEN
             bad_prf_Tvns = bad_prf_Tvns + 1
             prof%tag = 68
             CALL obs_rej%push_back(prof)
             prof%temp = PROF_UNDEF
          ENDIF ! (dvrs > prf_Trsmx)
       ENDIF ! (SIZE(prof%temp) >= 2)

       !------------------------------------------------------------------------
       !--- $ check spikiness in Salt
       !------------------------------------------------------------------------
       IF (SIZE(prof%salt) >= 2 .AND. prof%salt(1) /= PROF_UNDEF) THEN
          !-- dsdz at surface
          dsdz = (prof%salt(1)-prof%salt(2))/(prof%depth(2)-prof%depth(1))
          IF (ABS(dsdz) > prf_S1spmax) THEN
             !-dsdz, prof%salt(1),prof%salt(2),prof%salt(3)
             prof%salt(1) = prof%salt(2)
             bad_prf_S1sp = bad_prf_S1sp + 1
             prof%tag = 66
             CALL obs_rej%push_back(prof)
          ENDIF ! (ABS(dsdz) > prf_S1spmax)

          !-- dsdz at bottom
          kb = SIZE(prof%salt)
          dsdz = (prof%salt(kb-1)-prof%salt(kb))/(prof%depth(kb)-prof%depth(kb-1))
          IF (ABS(dsdz) > prf_Sbspmax) THEN
             !-dsdz, prof%salt(kb-2),prof%salt(kb-1),prof%salt(kb)
             bad_prf_Sbsp = bad_prf_Sbsp + 1
             prof%tag = 67
             CALL obs_rej%push_back(prof)
             prof%salt(kb) = PROF_UNDEF
          ENDIF ! (ABS(dsdz) > prf_Sbspmax)

          !-- noise check base on rms of dsdz**2
          dvns = 0.0
          kn = 0
          DO k = 2, SIZE(prof%salt)
             IF (prof%salt(k) /= PROF_UNDEF ) THEN
                dsdz = (prof%salt(k-1)-prof%salt(k))/(prof%depth(k)-prof%depth(k-1))
                dvns = dvns + dsdz**2
                kn = kn + 1
             ENDIF
          ENDDO !k = 2, SIZE(prof%salt)
          IF (kn == 0) kn = 1  ! just for check
          dvrs = SQRT(dvns/kn)
          IF (dvrs > prf_Srsmx) THEN
             bad_prf_Svns = bad_prf_Svns + 1
             prof%tag = 69
             CALL obs_rej%push_back(prof)
             DEALLOCATE(prof%salt)
             ALLOCATE(prof%salt(0))
          ENDIF ! (dvrs > prf_Srsmx)
       ENDIF ! (SIZE(prof%salt) >= 2)

       ! make sure there are either valid T or S values remaining
       IF (SIZE(prof%salt)+SIZE(prof%temp) > 0) THEN
          CALL obs_out%push_back(prof)
       END IF

    END DO main ! i = 1, obs_in%SIZE()
    !---

    IF(bad_prf_trconst_T > 0) &
         PRINT '(I8,A)', bad_prf_trconst_T, ' profiles removed temp for near constant T profile, h60'
    IF(bad_prf_trconst_S > 0) &
         PRINT '(I8,A)', bad_prf_trconst_S, ' profiles removed salt for near constant S profile, h61'
    IF(bad_prf_trconst > 0) &
         PRINT '(I8,A)', bad_prf_trconst, ' profiles removed for near constant T and S profile, h62'
    IF(bad_prf_T1sp > 0) &
         PRINT '(I8,A)', bad_prf_T1sp, ' profiles fixed Temp at surface for spikiness, h64'
    IF(bad_prf_Tbsp > 0) &
         PRINT '(I8,A)', bad_prf_Tbsp, ' profiles removed bottom Temp for spikiness, h65'
    IF(bad_prf_S1sp > 0) &
         PRINT '(I8,A)', bad_prf_S1sp, ' profiles fixed Salt at surface for spikiness, h66'
    IF(bad_prf_Sbsp > 0) &
         PRINT '(I8,A)', bad_prf_Sbsp, ' profiles removed bottom Salt for spikiness, h67'
    IF(bad_prf_Tvns > 0) &
         PRINT '(I8,A)', bad_prf_Tvns, ' profiles of T removed for noisy profile in rms of dtdz**2, h68'
    IF(bad_prf_Svns > 0) &
         PRINT '(I8,A)', bad_prf_Svns, ' profiles of S removed for noisy profile in rms of dsdz**2, h69'


  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_profile_mod
