!===============================================================================
!!  QC for globalgross check
!-------------------------------------------------------------------------------
MODULE qc_global_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod
  USE netcdf
  USE cubic_spline

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_global
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE, NOPASS :: init  => qc_step_init
     PROCEDURE, NOPASS :: check => qc_step_check
  END TYPE qc_global
  !=============================================================================

  ! parameters to be from namelist
  LOGICAL, PUBLIC :: do_qc_global = .TRUE.                  ! do this QC
  CHARACTER(len=80), PUBLIC :: cobs_cord = "SPHERICAL"      ! climate obs coordinate
  REAL, PUBLIC :: cobs_dx = 1.0                             ! delta x in longitude
  REAL, PUBLIC :: cobs_dy = 1.0                             ! delta y in latitude
  REAL, PUBLIC :: cobs_x0 = 0.5                             ! longitude of 1st grid
  REAL, PUBLIC :: cobs_gros_MIN_T = 2.0                     ! minimum of the gross off set check for temp
  REAL, PUBLIC :: cobs_gros_MAX_T = 20.0                    ! maximum of the gross off set check for temp
  REAL, PUBLIC :: cobs_gros_MIN_S = 0.3                     ! minimum of the gross off set check for salt
  REAL, PUBLIC :: cobs_gros_MAX_S = 3.0                     ! maximum of the gross off set check for salt
  INTEGER, PUBLIC :: cobs_fMAX = 2                          ! max number of serching sea grid in function sphgrid_lalo2xy
  REAL, PUBLIC :: cobs_gros_sdv_T = 5.0                     ! MAX number of SDV range for the gross check for temp
  REAL, PUBLIC :: cobs_gros_sdv_S = 5.0                     ! MAX number of SDV range for the gross check for salt
  INTEGER, PUBLIC :: cobs_nx                                ! x- dimension of cobs
  INTEGER, PUBLIC :: cobs_ny                                ! y- dimension of cobs
  INTEGER, PUBLIC :: cobs_nz                                ! z- dimension of cobs, layer
  INTEGER, PUBLIC :: cobs_nt                                ! t- dimension of cobs, months

  REAL, ALLOCATABLE, PUBLIC :: cobs_lon(:)           ! longitudes (degrees)
  REAL, ALLOCATABLE, PUBLIC :: cobs_lat(:)           ! latitudes (degrees)
  REAL, ALLOCATABLE, PUBLIC :: cobs_dep(:)           ! depths (meters)
  REAL, ALLOCATABLE, PUBLIC :: cobs_temp(:,:,:,:)    ! temp
  REAL, ALLOCATABLE, PUBLIC :: cobs_salt(:,:,:,:)    ! salt
  REAL, ALLOCATABLE, PUBLIC :: cobs_toff(:,:,:,:)    ! toff
  REAL, ALLOCATABLE, PUBLIC :: cobs_soff(:,:,:,:)    ! soff
  REAL, ALLOCATABLE, PUBLIC :: cobs_mask(:,:)        ! ocean mask

  TYPE(cspline) :: cobs_tspl, cobs_tspl_off, cobs_sspl, cobs_sspl_off

CONTAINS

  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !----------------------------------- ------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_global"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "This is a QC step for global checking"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only CALLed once, even if the qc_step_check
  !! subroutine is CALLed multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(nmlfile)
    INTEGER, INTENT(in) :: nmlfile
    INTEGER :: unit, ncid, vid
    INTEGER :: i, j, k
    REAL :: nbig = 1.0e5
    CHARACTER(len=:), ALLOCATABLE :: cobs_TS_file
    CHARACTER(len=:), ALLOCATABLE :: cobs_lon_dim
    CHARACTER(len=:), ALLOCATABLE :: cobs_lat_dim
    CHARACTER(len=:), ALLOCATABLE :: cobs_dep_dim
    CHARACTER(len=:), ALLOCATABLE :: cobs_mon_dim
    CHARACTER(len=:), ALLOCATABLE :: cobs_lon_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_lat_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_dep_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_temp_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_salt_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_toff_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_soff_var

    NAMELIST /qc_global/ do_qc_global, &
         cobs_cord, cobs_dx, cobs_dy, cobs_x0, cobs_fMAX, &
         cobs_gros_sdv_T, cobs_gros_MIN_T, cobs_gros_MAX_T, &
         cobs_gros_sdv_S, cobs_gros_MIN_S, cobs_gros_MAX_S, &
         cobs_TS_file, cobs_lon_dim, cobs_lat_dim, cobs_dep_dim, &
         cobs_mon_dim, cobs_lon_var, cobs_lat_var, cobs_dep_var, &
         cobs_temp_var, cobs_salt_var, cobs_toff_var, cobs_soff_var

    ALLOCATE(CHARACTER(len=1024) :: cobs_TS_file)
    ALLOCATE(CHARACTER(len=1024) :: cobs_lon_dim)
    ALLOCATE(CHARACTER(len=1024) :: cobs_lat_dim)
    ALLOCATE(CHARACTER(len=1024) :: cobs_dep_dim)
    ALLOCATE(CHARACTER(len=1024) :: cobs_mon_dim)
    ALLOCATE(CHARACTER(len=1024) :: cobs_lon_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_lat_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_dep_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_temp_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_salt_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_toff_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_soff_var)
    !---- read namelist from qc_global
    READ(nmlfile, qc_global)
    !----
    cobs_TS_file = TRIM(cobs_TS_file)
    cobs_lon_dim = TRIM(cobs_lon_dim)
    cobs_lat_dim = TRIM(cobs_lat_dim)
    cobs_dep_dim = TRIM(cobs_dep_dim)
    cobs_mon_dim = TRIM(cobs_mon_dim)
    cobs_lon_var = TRIM(cobs_lon_var)
    cobs_lat_var = TRIM(cobs_lat_var)
    cobs_dep_var = TRIM(cobs_dep_var)
    cobs_temp_var = TRIM(cobs_temp_var)
    cobs_salt_var = TRIM(cobs_salt_var)
    cobs_toff_var = TRIM(cobs_toff_var)
    cobs_soff_var = TRIM(cobs_soff_var)
    cobs_cord = TRIM(cobs_cord)
    !----
    PRINT qc_global
    !---- Now this global check is available for
    !     only the sherical coordinate of WOA13
    IF (TRIM(cobs_cord) .NE. "SPHERICAL") THEN
       PRINT *, "In qc_global, COBS should be SPHERICAL, Skip qc_global!"
       RETURN
    ENDIF
    !---- load WOA13 clomatologies of T and S (analysis)
    !---- get dimensions
    CALL check(nf90_open(cobs_TS_file, nf90_nowrite, ncid))
    CALL check(nf90_inq_dimid(ncid, cobs_lon_dim, vid))
    CALL check(nf90_inquire_dimension(ncid, vid, len=cobs_nx))
    CALL check(nf90_inq_dimid(ncid, cobs_lat_dim, vid))
    CALL check(nf90_inquire_dimension(ncid, vid, len=cobs_ny))
    CALL check(nf90_inq_dimid(ncid, cobs_dep_dim, vid))
    CALL check(nf90_inquire_dimension(ncid, vid, len=cobs_nz))
    CALL check(nf90_inq_dimid(ncid, cobs_mon_dim, vid))
    CALL check(nf90_inquire_dimension(ncid, vid, len=cobs_nt))
    !---- longitudes
    ALLOCATE(cobs_lon(cobs_nx))
    CALL check(nf90_inq_varid(ncid, cobs_lon_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_lon))
    !---- latitudes
    ALLOCATE(cobs_lat(cobs_ny))
    CALL check(nf90_inq_varid(ncid, cobs_lat_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_lat))
    !---- depths
    ALLOCATE(cobs_dep(cobs_nz))
    CALL check(nf90_inq_varid(ncid, cobs_dep_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_dep))
    !---- temp
    ALLOCATE(cobs_temp(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    CALL check(nf90_inq_varid(ncid, cobs_temp_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_temp))
    !---- salt
    ALLOCATE(cobs_salt(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    CALL check(nf90_inq_varid(ncid, cobs_salt_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_salt))
    !---- toff
    ALLOCATE(cobs_toff(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    CALL check(nf90_inq_varid(ncid, cobs_toff_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_toff))
    !---- soff
    ALLOCATE(cobs_soff(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    CALL check(nf90_inq_varid(ncid, cobs_soff_var, vid))
    CALL check(nf90_get_var(ncid, vid, cobs_soff))
    !----
    CALL check(nf90_close(ncid))

    !---- set cobs_mask
    ALLOCATE(cobs_mask(cobs_nx,cobs_ny))
    cobs_mask(:,:)=1.0
    DO j=1,cobs_ny
       DO i=1,cobs_nx
          IF (ABS(cobs_temp(i,j,1,1) ) .GE. nbig) cobs_mask(i,j)=0.0
       ENDDO
    ENDDO
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

    INTEGER :: i, j, k, btma, btmb, btmc, btmd, mon, mdt
    LOGICAL :: ck_srch
    TYPE(profile), POINTER :: prof

    ! read in from namelist
    INTEGER :: obid_t  = 2210
    INTEGER :: obid_pt = 2211
    INTEGER :: obid_s  = 2220
    REAL :: lat_bounds(2) = (/-90,90/)
    REAL :: alon, alat
    INTEGER :: bad_gb_outbound, bad_gb_noprof, bad_gb_gross
    INTEGER :: bad_gb_gross_T, bad_gb_noprof_off_T
    INTEGER :: bad_gb_gross_S, bad_gb_noprof_off_S
    INTEGER :: x, y, z
    REAL :: nbig = 1.0e5
    REAL :: hbig = 1.0e5
    REAL :: s, pt, v, odep, coff, cdif
    REAL, ALLOCATABLE :: cobs_tinp(:)
    REAL, ALLOCATABLE :: cobs_sinp(:)
    REAL, ALLOCATABLE :: cobs_tinp_off(:)
    REAL, ALLOCATABLE :: cobs_sinp_off(:)

    !----
    IF (.NOT. do_qc_global) THEN
       PRINT *, "Skip qc_global"
       obs_out=obs_in
       RETURN
    ENDIF
    !-------------
    bad_gb_outbound = 0
    bad_gb_noprof = 0
    bad_gb_gross = 0
    bad_gb_gross_T = 0
    bad_gb_noprof_off_T = 0
    bad_gb_gross_S = 0
    bad_gb_noprof_off_S = 0
    !-------------

    !--- main loop
    global : DO i = 1, obs_in%SIZE()
       prof => obs_in%of(i)
       !---
       IF (ALLOCATED(cobs_tinp)) DEALLOCATE(cobs_tinp)
       ALLOCATE(cobs_tinp(SIZE(prof%depth)))
       IF (ALLOCATED(cobs_tinp_off)) DEALLOCATE(cobs_tinp_off)
       ALLOCATE(cobs_tinp_off(SIZE(prof%depth)))
       IF (ALLOCATED(cobs_sinp)) DEALLOCATE(cobs_sinp)
       ALLOCATE(cobs_sinp(SIZE(prof%depth)))
       IF (ALLOCATED(cobs_sinp_off)) DEALLOCATE(cobs_sinp_off)
       ALLOCATE(cobs_sinp_off(SIZE(prof%depth)))
       !--- find nearest ocean point in cobs grid
       alon = prof%lon
       alat = prof%lat
       mdt = prof%date/100
       mon = MOD(mdt,100)
       !---> Assume one of two case
       !---> obs(-180 - 180) cobs(0 - 360) : WOA09
       !---> obs(-180 - 180) cobs(-180 - 180) : WOA13
       CALL sphgrid_lalo2xy(alon, alat, cobs_dx, cobs_dy, cobs_dx, x, y, ck_srch)

       IF (.NOT. ck_srch) THEN
          ! If no good closest point found
          bad_gb_outbound = bad_gb_outbound + 1
          prof%tag = 50
          CALL obs_rej%push_back(prof)
          CYCLE global

       ELSE ! (ck_srch)
          ! otherwise a good location was found...


          !---------------------------------------------------------------------
          ! temperature check
          !---------------------------------------------------------------------

          !--- find the bottom depth of cobs point
          DO btma = cobs_nz,1,-1
             IF (ABS(cobs_temp(x,y,btma,mon)) <= nbig) EXIT
          ENDDO
          DO btmc = cobs_nz,1,-1
             IF (ABS(cobs_salt(x,y,btmc,mon)) <= nbig) EXIT
          ENDDO
          !---
          IF (btma <= 1 .AND. btmc <= 1) THEN
             bad_gb_noprof = bad_gb_noprof + 1
             prof%tag = 52
             CALL obs_rej%push_back(prof)
             CYCLE global
          ENDIF

          !--- Interpolate to obs depth
          !--- obs T is in-situ T
          !-- TEMP check
          IF (btma > 1 .AND.  SIZE(prof%temp) .NE. 0) THEN
             cobs_tspl = cspline(cobs_dep(:btma), cobs_temp(x,y,:btma,mon))
             cobs_tinp = cobs_tspl%interp(prof%depth, check=.TRUE.)

             !--- toff
             !--- check bottom of off-set data
             DO btmb = cobs_nz,1,-1
                IF (ABS(cobs_toff(x,y,btmb,mon)) <= nbig) EXIT
             ENDDO
             !---
             IF (btmb <= 1) THEN
                bad_gb_noprof_off_T = bad_gb_noprof_off_T + 1
                prof%tag = 53
                CALL obs_rej%push_back(prof)
                CYCLE global
             ENDIF

             !--- Interpolate to obs depth
             cobs_tspl_off = cspline(cobs_dep(:btmb), cobs_toff(x,y,:btmb,mon))
             cobs_tinp_off = cobs_tspl_off%interp(prof%depth, check=.TRUE.)

             !==> gross check,
             vtemp : DO k = 1, SIZE(prof%temp)
                ! ignore this level if there is no defined temperature
                IF (prof%salt(k) == PROF_UNDEF) CYCLE

                cdif = ABS(prof%temp(k)-cobs_tinp(k))
                coff = MAX(cobs_gros_MIN_T, &
                     MIN(cobs_gros_MAX_T,cobs_gros_sdv_T*cobs_tinp_off(k)))

                IF (cdif > coff) THEN
                   ! level failing the gross check
                   ! TODO, add option to remove level, instead of flagging
                   ! entire variable bad
                   ! TODO, need to copy the profile before adding it to the
                   ! reject vector, otherwise the actual profile values will
                   ! not be retained (prof holds pointers to object, so the
                   ! contents of prof are currently being modified after
                   ! pushed back), same applies to salinity below.
                   bad_gb_gross_T = bad_gb_gross_T + 1
                   prof%tag = 51
                   CALL obs_rej%push_back(prof)
                   DEALLOCATE(prof%temp)
                   ALLOCATE(prof%temp(0))
                   EXIT vtemp
                ENDIF

             ENDDO vtemp ! k = 1, SIZE(prof%depth)
          END IF !(btma > 1)


          !---------------------------------------------------------------------
          ! salinity check
          !---------------------------------------------------------------------
          IF (btmc > 1 .AND.  SIZE(prof%salt) .NE. 0) THEN
             cobs_sspl = cspline(cobs_dep(:btmc), cobs_salt(x,y,:btmc,mon))
             cobs_sinp = cobs_sspl%interp(prof%depth, check=.TRUE.)

             !--- soff
             !--- check bottom of off-set data
             DO btmd = cobs_nz,1,-1
                IF (ABS(cobs_soff(x,y,btmd,mon)) <= nbig) EXIT
             ENDDO
             !---
             IF (btmd <= 1) THEN
                bad_gb_noprof_off_S = bad_gb_noprof_off_S + 1
                prof%tag = 54
                CALL obs_rej%push_back(prof)
                CYCLE global
             ENDIF

             !--- Interpolate to obs depth
             cobs_sspl_off = cspline(cobs_dep(:btmd), cobs_soff(x,y,:btmd,mon))
             cobs_sinp_off = cobs_sspl_off%interp(prof%depth, check=.TRUE.)

             !==> gross check,
             vsalt : DO k = 1, SIZE(prof%salt)

                ! ignore this level if there is no defined salinity
                IF (prof%salt(k) == PROF_UNDEF) CYCLE

                cdif = ABS(prof%salt(k)-cobs_sinp(k))
                coff = MAX(cobs_gros_MIN_S, &
                     MIN(cobs_gros_MAX_S,cobs_gros_sdv_S*cobs_sinp_off(k)))

                IF (cdif > coff) THEN
                   ! level failing the gross check
                   ! TODO, add option to remove level, instead of flagging
                   ! entire variable bad
                   bad_gb_gross_S = bad_gb_gross_S + 1
                   prof%tag = 55
                   CALL obs_rej%push_back(prof)
                   DEALLOCATE(prof%salt)
                   ALLOCATE(prof%salt(0))
                   EXIT vsalt
                ENDIF ! (cdif > coff)

             ENDDO vsalt ! k = 1, SIZE(prof%depth)
          END IF !(btmc > 1)

          !--- keep T or S if good, and reject if both bad.
          !--- rej file of rejected T or S can be bigger than REAL rejected profile of both bad.
          IF (SIZE(prof%temp) == 0 .and. SIZE(prof%salt) == 0) THEN
             bad_gb_gross = bad_gb_gross + 1
             prof%tag = 56
             CALL obs_rej%push_back(prof)
             CYCLE global
          ENDIF

       ENDIF ! (ck_srch)

       CALL obs_out%push_back(prof)

       IF(ALLOCATED(cobs_tinp)) DEALLOCATE(cobs_tinp)
       IF(ALLOCATED(cobs_tinp_off)) DEALLOCATE(cobs_tinp_off)
       IF(ALLOCATED(cobs_sinp)) DEALLOCATE(cobs_sinp)
       IF(ALLOCATED(cobs_sinp_off)) DEALLOCATE(cobs_sinp_off)

    END DO global ! i = 1, obs_in%SIZE()


    ! print out stats if any profiles were removed
    ! NOTE: these should be printed in the order that they were checked
    IF(bad_gb_outbound > 0) &
         PRINT '(I8,A)', bad_gb_outbound, ' profiles removed for no near ocean point for Clim Obs in qc_global, h50'
    IF(bad_gb_noprof > 0) &
         PRINT '(I8,A)', bad_gb_noprof, ' profiles removed :: too shallow in cobs, h52'
    IF(bad_gb_gross > 0) &
         PRINT '(I8,A)', bad_gb_gross, ' total profiles removed for out of T/S gross check in qc_global, h56'
    IF(bad_gb_gross_T > 0) &
         PRINT '(I8,A)', bad_gb_gross_T, ' profiles removed for out of T gross check in qc_global, h51'
    IF(bad_gb_noprof_off_T > 0) &
         PRINT '(I8,A)', bad_gb_noprof_off_T, ' profiles removed T_off at too shallow in cobs, h53'
    IF(bad_gb_gross_S > 0) &
         PRINT '(I8,A)', bad_gb_gross_S, ' profiles removed for out of S gross check in qc_global, h55'
    IF(bad_gb_noprof_off_S > 0) &
         PRINT '(I8,A)', bad_gb_noprof_off_S, ' profiles removed S_off at too shallow in cobs, h54'
    !---

  END SUBROUTINE qc_step_check

  !---------------------------

  SUBROUTINE sphgrid_lalo2xy &
       (alon, alat, cobs_dx, cobs_dy, cobs_x0, ix,jy,srch)
    REAL, INTENT(in) :: alon, alat, cobs_dx, cobs_dy, cobs_x0
    INTEGER, INTENT(inout) :: ix,jy
    LOGICAL, INTENT(inout) :: srch
    REAL :: plon, plat, pxmd, pymd
    INTEGER :: nx, ny, n, is, ie, js, je, i, j
    !-- Find nearest grid of cobs (i and j), assumed to the shperical coordinate
    !-- add 180.0 and 90.0, to make mapping from lat-lon to grid of shperical coord.
    !-- (both longitude and latitude of obs turn to positive (now -180, -90))
    IF (cobs_x0 < 0) THEN
       !-- (-180 - 180) -> (-180 - 180)
       plon = alon + 180.0 + 0.5*cobs_dx
    ELSE ! (cobs_x0 < 0)
       !-- (-180 - 180)  -> (0 - 360)
       IF (alon < 0) THEN
          plon = alon + 360.0 + 0.5*cobs_dx
       ELSE
          plon = alon + 0.5*cobs_dx
       ENDIF
    ENDIF ! (cobs_x0 < 0)
    !--
    plat = alat + 90.0 + 0.5*cobs_dy
    pxmd = amod(plon,cobs_dx)
    pymd = amod(plat,cobs_dy)
    nx = INT(plon/cobs_dx)
    ny = INT(plat/cobs_dy)
    IF (pxmd >= 0.5*cobs_dx) nx=nx+1
    IF (pymd >= 0.5*cobs_dy) ny=ny+1
    !--- serching the nearest ocean grid in cobs
    IF (cobs_mask(nx,ny) == 1) THEN
       ix = nx
       jy = ny
       srch = .TRUE.
    ELSE ! (cobs_mask(nx,ny) == 1)
       !-- serching inner loop
       inner1 : DO n = 1, cobs_fMAX
          is = nx - n
          ie = nx + n
          js = ny - n
          je = ny + n
          !-- check border
          IF (is <= 1) is =1
          IF (js <= 1) js =1
          IF (ie >= cobs_nx) ie = cobs_nx
          IF (je >= cobs_ny) je = cobs_ny
          DO j = js, je
             DO i= is, ie
                IF (cobs_mask(i,j) == 1) THEN
                   ix = i
                   jy = j
                   srch = .TRUE.
                   CYCLE inner1
                ENDIF ! (cobs_mask(i,j) == 1)
             ENDDO ! i= is, ie
          ENDDO ! j = js, je
          !-- no nearest ocean point
          ix=9999
          jy=9999
          srch = .FALSE.
       END DO inner1 ! n = 1, cobs_fMAX
    ENDIF ! (cobs_mask(nx,ny) == 1)

  END SUBROUTINE sphgrid_lalo2xy

  !---------------------------

  SUBROUTINE check(status)
    INTEGER, INTENT(in) :: status
    IF(status /= nf90_noerr) THEN
       PRINT *, TRIM(nf90_strerror(status))
       STOP 1
    END IF
  END SUBROUTINE check


  !=============================================================================

END MODULE qc_global_mod
