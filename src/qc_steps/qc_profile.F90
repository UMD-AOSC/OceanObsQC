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
     PROCEDURE         :: init  => qc_step_init
     PROCEDURE         :: check => qc_step_check
  END TYPE qc_profile
  !=============================================================================

  ! parameters to be from namelist
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
  SUBROUTINE qc_step_init(self, nmlfile)
    CLASS(qc_profile) :: self
    INTEGER, INTENT(in) :: nmlfile

    !NAMELIST /QCTEMPLATE/ var1, var2
    !READ(nmlfile, QCTEMPLATE)
    !PRINT QCTEMPLATE

    NAMELIST /qc_profile/  &
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
  SUBROUTINE qc_step_check(self, obs_in, obs_out, obs_rej)
    CLASS(qc_profile) :: self
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    INTEGER :: i, k, kb, kn
    REAL :: trTmin, trTmax, trSmin, trSmax, dtdz, dsdz, dvns, dvrs
    TYPE(profile),POINTER :: prof
    TYPE(profile) :: prof_rej

    INTEGER :: bad_prf_trconst_T, bad_prf_trconst_S
    INTEGER :: bad_prf_T1sp, bad_prf_Tbsp, bad_prf_S1sp, bad_prf_Sbsp
    INTEGER :: bad_prf_Tvns, bad_prf_Svns

    INTEGER :: tag_prf_trconst_T, tag_prf_trconst_S
    INTEGER :: tag_prf_T1sp, tag_prf_Tbsp, tag_prf_S1sp, tag_prf_Sbsp
    INTEGER :: tag_prf_Tvns, tag_prf_Svns

    !---
    bad_prf_trconst_T = 0
    bad_prf_trconst_S = 0
    bad_prf_T1sp = 0
    bad_prf_Tbsp = 0
    bad_prf_S1sp = 0
    bad_prf_Sbsp = 0
    bad_prf_Tvns = 0
    bad_prf_Svns = 0

    tag_prf_trconst_T = self%err_base + 1
    tag_prf_trconst_S = self%err_base + 2
    tag_prf_T1sp      = self%err_base + 3
    tag_prf_Tbsp      = self%err_base + 4
    tag_prf_S1sp      = self%err_base + 5
    tag_prf_Sbsp      = self%err_base + 6
    tag_prf_Tvns      = self%err_base + 7
    tag_prf_Svns      = self%err_base + 8

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
                bad_prf_trconst_T = bad_prf_trconst_T + 1
                prof_rej = prof%copy('T')
                CALL prof%clear('T')
                prof_rej%tag = tag_prf_trconst_T
                CALL obs_rej%push_back(prof_rej)
             END IF
          ENDIF

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
                prof_rej = prof%copy('S')
                CALL prof%clear('S')
                prof_rej%tag = tag_prf_trconst_S
                CALL obs_rej%push_back(prof_rej)
             ENDIF
          ENDIF

          ! Make sure a T or S profile still exists
          IF(SIZE(prof%salt) == 0 .AND. SIZE(prof%temp) == 0) CYCLE main
       ENDIF


       !------------------------------------------------------------------------
       !--- $ check spikiness in Temp
       ! TODO this logic does not work if there are missing levels in Temp
       !------------------------------------------------------------------------
       spike_temp: IF (SIZE(prof%temp) >= 2 .AND. prof%temp(1) /= PROF_UNDEF) THEN

          !-- dtdz at surface
          dtdz = (prof%temp(1)-prof%temp(2))/(prof%depth(2)-prof%depth(1))
          IF (ABS(dtdz) > prf_T1spmax) THEN
             !-dtdz, prof%temp(1),prof%temp(2),prof%temp(3)
             prof%temp(1) = prof%temp(2)
             bad_prf_T1sp = bad_prf_T1sp + 1
             prof_rej = prof%copy('T')
             prof_rej%tag = tag_prf_T1sp
             CALL obs_rej%push_back(prof_rej)
          ENDIF ! (ABS(dtdz) > prf_T1spmax)

          !-- dtdz at bottom
          kb = SIZE(prof%temp)
          dtdz = (prof%temp(kb-1)-prof%temp(kb))/(prof%depth(kb)-prof%depth(kb-1))
          IF (ABS(dtdz) > prf_Tbspmax) THEN
             !-dtdz, prof%temp(kb-2),prof%temp(kb-1),prof%temp(kb)
             bad_prf_Tbsp = bad_prf_Tbsp + 1
             prof_rej = prof%copy('T')
             prof_rej%tag = tag_prf_Tbsp
             CALL obs_rej%push_back(prof_rej)
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
             prof_rej = prof%copy('T')
             prof_rej%tag = tag_prf_Tvns
             CALL obs_rej%push_back(prof_rej)
             CALL prof%clear('T')
          ENDIF ! (dvrs > prf_Trsmx)
       ENDIF spike_temp ! (SIZE(prof%temp) >= 2)

       !------------------------------------------------------------------------
       !--- $ check spikiness in Salt
       !------------------------------------------------------------------------
       spike_salt: IF (SIZE(prof%salt) >= 2 .AND. prof%salt(1) /= PROF_UNDEF) THEN
          !-- dsdz at surface
          dsdz = (prof%salt(1)-prof%salt(2))/(prof%depth(2)-prof%depth(1))
          IF (ABS(dsdz) > prf_S1spmax) THEN
             !-dsdz, prof%salt(1),prof%salt(2),prof%salt(3)
             prof%salt(1) = prof%salt(2)
             bad_prf_S1sp = bad_prf_S1sp + 1
             prof_rej = prof%copy('S')
             prof_rej%tag = tag_prf_S1sp
             CALL obs_rej%push_back(prof_rej)
          ENDIF ! (ABS(dsdz) > prf_S1spmax)

          !-- dsdz at bottom
          kb = SIZE(prof%salt)
          dsdz = (prof%salt(kb-1)-prof%salt(kb))/(prof%depth(kb)-prof%depth(kb-1))
          IF (ABS(dsdz) > prf_Sbspmax) THEN
             !-dsdz, prof%salt(kb-2),prof%salt(kb-1),prof%salt(kb)
             bad_prf_Sbsp = bad_prf_Sbsp + 1
             prof_rej = prof%copy('S')
             prof_rej%tag = tag_prf_Sbsp
             CALL obs_rej%push_back(prof_rej)
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
             prof_rej = prof%copy('S')
             prof_rej%tag = tag_prf_Svns
             CALL obs_rej%push_back(prof_rej)
             CALL prof%clear('S')
          ENDIF ! (dvrs > prf_Srsmx)
       ENDIF spike_salt ! (SIZE(prof%salt) >= 2)

       ! Make sure a T or S profile still exists
       IF(SIZE(prof%salt) == 0 .AND. SIZE(prof%temp) == 0) CYCLE main

       ! if we made it this far, the profile is good
       CALL obs_out%push_back(prof)

    END DO main ! i = 1, obs_in%SIZE()
    !---


    CALL print_rej_count(bad_prf_trconst_T, 'T profiles removed for near constant T profile', tag_prf_trconst_T)
    CALL print_rej_count(bad_prf_trconst_S, 'S profiles removed for near constant S profile', tag_prf_trconst_T)
    CALL print_rej_count(bad_prf_T1sp, 'T profiles fixed at surface for spikiness', tag_prf_T1sp)
    CALL print_rej_count(bad_prf_Tbsp, 'T profiles fixed at bottom for spikiness', tag_prf_Tbsp)
    CALL print_rej_count(bad_prf_S1sp, 'S profiles fixed at surface for spikiness', tag_prf_S1sp)
    CALL print_rej_count(bad_prf_Sbsp, 'S profiles fixed at bottom for spikiness', tag_prf_Sbsp)
    CALL print_rej_count(bad_prf_Tvns, 'T profiles removed for noisy rms of dtdz**2', tag_prf_Tvns)
    CALL print_rej_count(bad_prf_Svns, 'S profiles removed for noisy rms of dsdz**2', tag_prf_Svns)


  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_profile_mod
