!===============================================================================
!!  QC for globalgross check
!-------------------------------------------------------------------------------
MODULE qc_clim_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod
  USE netcdf
  USE cubic_spline
  USE kdtree

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_clim
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE         :: init  => qc_step_init
     PROCEDURE         :: check => qc_step_check
  END TYPE qc_clim
  !=============================================================================


  ! parameters to be read from namelist
  REAL :: clim_searchdist = 200e3
  REAL :: cobs_gros_MIN_T = 2.0                     ! minimum of the gross off set check for temp
  REAL :: cobs_gros_MAX_T = 20.0                    ! maximum of the gross off set check for temp
  REAL :: cobs_gros_MIN_S = 0.3                     ! minimum of the gross off set check for salt
  REAL :: cobs_gros_MAX_S = 3.0                     ! maximum of the gross off set check for salt
  REAL :: cobs_gros_sdv_T = 5.0                     ! MAX number of SDV range for the gross check for temp
  REAL :: cobs_gros_sdv_S = 5.0                     ! MAX number of SDV range for the gross check for salt

  ! T/S climatology file
  INTEGER :: cobs_nx                                ! x- dimension of cobs
  INTEGER :: cobs_ny                                ! y- dimension of cobs
  INTEGER :: cobs_nz                                ! z- dimension of cobs, layer
  INTEGER :: cobs_nt                                ! t- dimension of cobs, months
  REAL, ALLOCATABLE :: cobs_lon(:)           ! longitudes (degrees)
  REAL, ALLOCATABLE :: cobs_lat(:)           ! latitudes (degrees)
  REAL, ALLOCATABLE :: cobs_dep(:)           ! depths (meters)
  REAL, ALLOCATABLE :: cobs_temp(:,:,:,:)    ! temp
  REAL, ALLOCATABLE :: cobs_salt(:,:,:,:)    ! salt
  REAL, ALLOCATABLE :: cobs_toff(:,:,:,:)    ! toff
  REAL, ALLOCATABLE :: cobs_soff(:,:,:,:)    ! soff
  REAL, ALLOCATABLE :: cobs_mask(:,:)        ! ocean mask

  ! kdtree variables for fast closest-gridpoint lookups of T/S clim grid
  TYPE(kd_root) :: kd_tree
  INTEGER :: kd_tree_cnt
  INTEGER, ALLOCATABLE :: kd_tree_x(:), kd_tree_y(:)


  REAL, PARAMETER :: nbig = 1.0e5


CONTAINS

  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !----------------------------------- ------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_clim"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "compare against T/S climatology mean and standard deviation"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only CALLed once, even if the qc_step_check
  !! subroutine is CALLed multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(self, nmlfile)
    CLASS(qc_clim) :: self
    INTEGER, INTENT(in) :: nmlfile

    INTEGER :: unit, ncid, vid
    INTEGER :: i, j, k, x, y
    INTEGER :: i4(4)

    CHARACTER(len=:), ALLOCATABLE :: cobs_TS_file
    CHARACTER(len=:), ALLOCATABLE :: cobs_lon_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_lat_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_dep_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_temp_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_salt_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_toff_var
    CHARACTER(len=:), ALLOCATABLE :: cobs_soff_var

    REAL, ALLOCATABLE :: kd_tree_lons(:), kd_tree_lats(:)

    NAMELIST /qc_clim/ &
         cobs_gros_sdv_T, cobs_gros_MIN_T, cobs_gros_MAX_T, &
         cobs_gros_sdv_S, cobs_gros_MIN_S, cobs_gros_MAX_S, &
         cobs_TS_file, cobs_lon_var, cobs_lat_var, cobs_dep_var, &
         clim_searchdist, &
         cobs_temp_var, cobs_salt_var, cobs_toff_var, cobs_soff_var

    ! read namelist parameters
    ALLOCATE(CHARACTER(len=1024) :: cobs_TS_file)
    ALLOCATE(CHARACTER(len=1024) :: cobs_lon_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_lat_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_dep_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_temp_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_salt_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_toff_var)
    ALLOCATE(CHARACTER(len=1024) :: cobs_soff_var)
    READ(nmlfile, qc_clim)
    cobs_TS_file = TRIM(cobs_TS_file)
    cobs_lon_var = TRIM(cobs_lon_var)
    cobs_lat_var = TRIM(cobs_lat_var)
    cobs_dep_var = TRIM(cobs_dep_var)
    cobs_temp_var = TRIM(cobs_temp_var)
    cobs_salt_var = TRIM(cobs_salt_var)
    cobs_toff_var = TRIM(cobs_toff_var)
    cobs_soff_var = TRIM(cobs_soff_var)
    PRINT qc_clim


    !---- load WOA13 clomatologies of T and S (analysis)
    ! TODO add the option for using 2D lat/lon grids?
    ! --------------------------------------------------------------------------
    CALL check(nf90_open(cobs_TS_file, nf90_nowrite, ncid))

    !---- longitudes
    CALL check(nf90_inq_varid(ncid, cobs_lon_var, vid))
    CALL check(nf90_inquire_variable(ncid, vid, ndims=i))
    IF(i/=1) THEN
       PRINT *, "ERROR: climatology file must have a 1D longitude"
       STOP 1
    END IF
    CALL check(nf90_inquire_variable(ncid, vid, dimids=i4))
    CALL check(nf90_inquire_dimension(ncid, i4(1), len=cobs_nx))
    ALLOCATE(cobs_lon(cobs_nx))
    CALL check(nf90_get_var(ncid, vid, cobs_lon))

    !---- latitudes
    CALL check(nf90_inq_varid(ncid, cobs_lat_var, vid))
    CALL check(nf90_inquire_variable(ncid, vid, ndims=i))
    IF(i/=1) THEN
       PRINT *, "ERROR: climatology file must have a 1D latitude"
       STOP 1
    END IF
    CALL check(nf90_inquire_variable(ncid, vid, dimids=i4))
    CALL check(nf90_inquire_dimension(ncid, i4(1), len=cobs_ny))
    ALLOCATE(cobs_lat(cobs_ny))
    CALL check(nf90_get_var(ncid, vid, cobs_lat))

    !---- depths
    CALL check(nf90_inq_varid(ncid, cobs_dep_var, vid))
    CALL check(nf90_inquire_variable(ncid, vid, ndims=i))
    IF(i/=1) THEN
       PRINT *, "ERROR: climatology file must have a 1D depth"
       STOP 1
    END IF
    CALL check(nf90_inquire_variable(ncid, vid, dimids=i4))
    CALL check(nf90_inquire_dimension(ncid, i4(1), len=cobs_nz))
    ALLOCATE(cobs_dep(cobs_nz))
    CALL check(nf90_get_var(ncid, vid, cobs_dep))

    !---- temp (and a check to make sure fields are monthly clim, ie 12 of them)
    CALL check(nf90_inq_varid(ncid, cobs_temp_var, vid))
    CALL check(nf90_inquire_variable(ncid, vid, ndims=i))
    IF(i/=4) THEN
       PRINT *, "ERROR: a monthly climatology is required"
       STOP 1
    END IF
    CALL check(nf90_inquire_variable(ncid, vid, dimids=i4))
    CALL check(nf90_inquire_dimension(ncid, i4(4), len=cobs_nt))
    IF (cobs_nt /= 12) THEN
       PRINT *, cobs_nt
       PRINT *, "ERROR: a monthly climatology is required"
       STOP 1
    END IF
    ALLOCATE(cobs_temp(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
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


    ! generate kdtree for subsequent look up of closest ocean gridpoint
    ALLOCATE(kd_tree_x(cobs_nx*cobs_ny))
    ALLOCATE(kd_tree_y(cobs_nx*cobs_ny))
    ALLOCATE(kd_tree_lons(cobs_nx*cobs_ny))
    ALLOCATE(kd_tree_lats(cobs_nx*cobs_ny))
    kd_tree_cnt = 0
    DO x=1,cobs_nx
       DO y = 1, cobs_ny
          IF(cobs_mask(x,y) == 0.0) CYCLE ! ignore land points
          kd_tree_cnt = kd_tree_cnt + 1
          kd_tree_lats(kd_tree_cnt) = cobs_lat(y)
          kd_tree_lons(kd_tree_cnt) = cobs_lon(x)
          kd_tree_x(kd_tree_cnt) = x
          kd_tree_y(kd_tree_cnt) = y
       END DO
    END DO
    CALL kd_init(kd_tree, kd_tree_lons(1:kd_tree_cnt), kd_tree_lats(1:kd_tree_cnt))
    DEALLOCATE(kd_tree_lats)
    DEALLOCATE(kd_tree_lons)

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
    CLASS(qc_clim) :: self
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    INTEGER :: i, j, k, btma, btmb, btmc, btmd, mon
    TYPE(profile), POINTER :: prof
    TYPE(profile) :: prof2

    INTEGER :: bad_outbound, bad_noprof
    INTEGER :: bad_gross_T, bad_noprof_off_T
    INTEGER :: bad_gross_S, bad_noprof_off_S
    INTEGER :: tag_outbound, tag_noprof
    INTEGER :: tag_gross_T, tag_noprof_off_T
    INTEGER :: tag_gross_S, tag_noprof_off_S

    INTEGER :: x, y, z
    REAL :: cdif, coff
    REAL, ALLOCATABLE :: cobs_tinp(:)
    REAL, ALLOCATABLE :: cobs_sinp(:)
    REAL, ALLOCATABLE :: cobs_tinp_off(:)
    REAL, ALLOCATABLE :: cobs_sinp_off(:)

    INTEGER :: r_points(1), r_num
    REAL :: r_dist(1)

    TYPE(cspline) :: cobs_tspl, cobs_tspl_off, cobs_sspl, cobs_sspl_off

    ! initialize bad obs counts
    bad_outbound = 0
    bad_noprof = 0
    bad_gross_T = 0
    bad_noprof_off_T = 0
    bad_gross_S = 0
    bad_noprof_off_S = 0

    tag_outbound     = self%err_base + 1
    tag_noprof       = self%err_base + 2
    tag_gross_T      = self%err_base + 3
    tag_noprof_off_T = self%err_base + 4
    tag_gross_S      = self%err_base + 5
    tag_noprof_off_S = self%err_base + 6


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

       mon = MOD(prof%date/100,100)

       !--- find nearest ocean point in cobs grid
       CALL kd_search_nnearest(kd_tree, REAL(prof%lon), REAL(prof%lat), 1, &
            r_points, r_dist, r_num, .FALSE.)
       x = kd_tree_x(r_points(1))
       y = kd_tree_y(r_points(1))

       ! If no good closest point found
       IF (r_dist(1) > clim_searchdist) THEN
          bad_outbound = bad_outbound + 1
          prof%tag = tag_outbound
          CALL obs_rej%push_back(prof)
          CYCLE global
       END IF


       ! otherwise a good location was found, continue...


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
          bad_noprof = bad_noprof + 1
          prof%tag = tag_noprof
          CALL obs_rej%push_back(prof)
          CYCLE global
       ENDIF

       !--- Interpolate to obs depth
       !--- obs T is in-situ T
       !-- TEMP check
       temp_if: IF (btma > 1 .AND.  SIZE(prof%temp) .NE. 0) THEN
          cobs_tspl = cspline(cobs_dep(:btma), cobs_temp(x,y,:btma,mon))
          cobs_tinp = cobs_tspl%interp(prof%depth, check=.TRUE.)

          !--- toff
          !--- check bottom of off-set data
          DO btmb = cobs_nz,1,-1
             IF (ABS(cobs_toff(x,y,btmb,mon)) <= nbig) EXIT
          ENDDO
          !---
          IF (btmb <= 1) THEN
             bad_noprof_off_T = bad_noprof_off_T + 1
             prof2 = prof%copy('T')
             prof2%tag = tag_noprof_off_T
             CALL prof%clear('T')
             CALL obs_rej%push_back(prof2)
             EXIT temp_if
          ENDIF

          !--- Interpolate to obs depth
          cobs_tspl_off = cspline(cobs_dep(:btmb), cobs_toff(x,y,:btmb,mon))
          cobs_tinp_off = cobs_tspl_off%interp(prof%depth, check=.TRUE.)

          !==> gross check,
          vtemp : DO k = 1, SIZE(prof%temp)
             ! ignore this level if there is no defined temperature
             IF (prof%temp(k) == PROF_UNDEF) CYCLE

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
                bad_gross_T = bad_gross_T + 1
                prof2 = prof%copy('T')
                prof2%tag = tag_gross_T
                CALL prof%clear('T')
                CALL obs_rej%push_back(prof2)
                EXIT vtemp
             ENDIF
          ENDDO vtemp ! k = 1, SIZE(prof%depth)

       END IF temp_if !(btma > 1)


       !---------------------------------------------------------------------
       ! salinity check
       !---------------------------------------------------------------------
       salt_if: IF (btmc > 1 .AND.  SIZE(prof%salt) .NE. 0) THEN
          cobs_sspl = cspline(cobs_dep(:btmc), cobs_salt(x,y,:btmc,mon))
          cobs_sinp = cobs_sspl%interp(prof%depth, check=.TRUE.)

          !--- soff
          !--- check bottom of off-set data
          DO btmd = cobs_nz,1,-1
             IF (ABS(cobs_soff(x,y,btmd,mon)) <= nbig) EXIT
          ENDDO
          !---
          IF (btmd <= 1) THEN
             bad_noprof_off_S = bad_noprof_off_S + 1
             prof2 = prof%copy('S')
             CALL prof%clear('S')
             prof2%tag = tag_noprof_off_S
             CALL obs_rej%push_back(prof2)
             EXIT salt_if
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
                bad_gross_S = bad_gross_S + 1
                prof2 = prof%copy('S')
                prof2%tag = tag_gross_S
                CALL prof%clear('S')
                CALL obs_rej%push_back(prof2)
                EXIT vsalt
             ENDIF ! (cdif > coff)

          ENDDO vsalt ! k = 1, SIZE(prof%depth)
       END IF salt_if !(btmc > 1)

       ! if both T and S are empty, abandon this profile
       IF (SIZE(prof%temp) == 0 .AND. SIZE(prof%salt) == 0) CYCLE

       ! IF we get to here, the profile is good
       CALL obs_out%push_back(prof)

       IF(ALLOCATED(cobs_tinp)) DEALLOCATE(cobs_tinp)
       IF(ALLOCATED(cobs_tinp_off)) DEALLOCATE(cobs_tinp_off)
       IF(ALLOCATED(cobs_sinp)) DEALLOCATE(cobs_sinp)
       IF(ALLOCATED(cobs_sinp_off)) DEALLOCATE(cobs_sinp_off)

    END DO global ! i = 1, obs_in%SIZE()


    ! print out stats if any profiles were removed
    ! NOTE: these should be printed in the order that they were checked
    CALL print_rej_count(bad_outbound, 'profiles removed because no nearby ocean points in climatology file', tag_outbound)
    CALL print_rej_count(bad_noprof, 'profiles removed because climatology file too shallow', tag_noprof)
    CALL print_rej_count(bad_noprof_off_T, 'T profiles removed because T_off too shallow in climatology file', tag_noprof_off_T)
    CALL print_rej_count(bad_gross_T, 'T profiles removed for T gross check', tag_gross_T)
    CALL print_rej_count(bad_noprof_off_S, 'S profiles removed because S_off too shallow in climatology file', tag_noprof_off_S)
    CALL print_rej_count(bad_gross_S, 'S profiles removed for S gross check', tag_gross_S)

    !---
  END SUBROUTINE qc_step_check




  SUBROUTINE check(status)
    INTEGER, INTENT(in) :: status
    IF(status /= nf90_noerr) THEN
       PRINT *, TRIM(nf90_strerror(status))
       STOP 1
    END IF
  END SUBROUTINE check


  !=============================================================================

END MODULE qc_clim_mod
