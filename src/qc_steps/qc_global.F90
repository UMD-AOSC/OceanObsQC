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
  LOGICAL, public :: do_qc_global = .TRUE.                  ! do this QC
  character(len=80), public :: cobs_cord = "SPHERICAL"      ! climate obs coordinate
  real, public :: cobs_dx = 1.0                             ! delta x in longitude
  real, public :: cobs_dy = 1.0                             ! delta y in latitude
  real, public :: cobs_x0 = 0.5                             ! longitude of 1st grid
  real, public :: cobs_gros_min_T = 2.0                     ! minimum of the gross off set check for temp
  real, public :: cobs_gros_max_T = 20.0                    ! maximum of the gross off set check for temp
  real, public :: cobs_gros_min_S = 0.3                     ! minimum of the gross off set check for salt
  real, public :: cobs_gros_max_S = 3.0                     ! maximum of the gross off set check for salt
  integer, public :: cobs_fmax = 2                          ! max number of serching sea grid in function sphgrid_lalo2xy
  real, public :: cobs_gros_sdv_T = 5.0                     ! max number of SDV range for the gross check for temp
  real, public :: cobs_gros_sdv_S = 5.0                     ! max number of SDV range for the gross check for salt
  integer, public :: cobs_nx                                ! x- dimension of cobs
  integer, public :: cobs_ny                                ! y- dimension of cobs
  integer, public :: cobs_nz                                ! z- dimension of cobs, layer
  integer, public :: cobs_nt                                ! t- dimension of cobs, months

  real, allocatable, public :: cobs_lon(:)           ! longitudes (degrees)
  real, allocatable, public :: cobs_lat(:)           ! latitudes (degrees)
  real, allocatable, public :: cobs_dep(:)           ! depths (meters)
  real, allocatable, public :: cobs_temp(:,:,:,:)    ! temp
  real, allocatable, public :: cobs_salt(:,:,:,:)    ! salt
  real, allocatable, public :: cobs_toff(:,:,:,:)    ! toff
  real, allocatable, public :: cobs_soff(:,:,:,:)    ! soff
  real, allocatable, public :: cobs_mask(:,:)        ! ocean mask

  type(cspline) :: cobs_tspl, cobs_tspl_off, cobs_sspl, cobs_sspl_off

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
  !! This subroutine is only called once, even if the qc_step_check
  !! subroutine is called multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(nmlfile)
    INTEGER, INTENT(in) :: nmlfile
    integer :: unit, ncid, vid
    integer :: i, j, k
    real :: nbig = 1.0e5
    character(len=:), allocatable :: cobs_TS_file
    character(len=:), allocatable :: cobs_lon_dim
    character(len=:), allocatable :: cobs_lat_dim
    character(len=:), allocatable :: cobs_dep_dim
    character(len=:), allocatable :: cobs_mon_dim
    character(len=:), allocatable :: cobs_lon_var
    character(len=:), allocatable :: cobs_lat_var
    character(len=:), allocatable :: cobs_dep_var
    character(len=:), allocatable :: cobs_temp_var
    character(len=:), allocatable :: cobs_salt_var
    character(len=:), allocatable :: cobs_toff_var
    character(len=:), allocatable :: cobs_soff_var

    NAMELIST /qc_global/ do_qc_global, &
      cobs_cord, cobs_dx, cobs_dy, cobs_x0, cobs_fmax, &
      cobs_gros_sdv_T, cobs_gros_min_T, cobs_gros_max_T, &
      cobs_gros_sdv_S, cobs_gros_min_S, cobs_gros_max_S, &
      cobs_TS_file, cobs_lon_dim, cobs_lat_dim, cobs_dep_dim, &
      cobs_mon_dim, cobs_lon_var, cobs_lat_var, cobs_dep_var, &
      cobs_temp_var, cobs_salt_var, cobs_toff_var, cobs_soff_var
       
    allocate(character(len=1024) :: cobs_TS_file)
    allocate(character(len=1024) :: cobs_lon_dim)
    allocate(character(len=1024) :: cobs_lat_dim)
    allocate(character(len=1024) :: cobs_dep_dim)
    allocate(character(len=1024) :: cobs_mon_dim)
    allocate(character(len=1024) :: cobs_lon_var)
    allocate(character(len=1024) :: cobs_lat_var)
    allocate(character(len=1024) :: cobs_dep_var)
    allocate(character(len=1024) :: cobs_temp_var)
    allocate(character(len=1024) :: cobs_salt_var)
    allocate(character(len=1024) :: cobs_toff_var)
    allocate(character(len=1024) :: cobs_soff_var)
!---- read namelist from qc_global
    READ(nmlfile, qc_global)
!----
    cobs_TS_file = trim(cobs_TS_file)
    cobs_lon_dim = trim(cobs_lon_dim)
    cobs_lat_dim = trim(cobs_lat_dim)
    cobs_dep_dim = trim(cobs_dep_dim)
    cobs_mon_dim = trim(cobs_mon_dim)
    cobs_lon_var = trim(cobs_lon_var)
    cobs_lat_var = trim(cobs_lat_var)
    cobs_dep_var = trim(cobs_dep_var)
    cobs_temp_var = trim(cobs_temp_var)
    cobs_salt_var = trim(cobs_salt_var)
    cobs_toff_var = trim(cobs_toff_var)
    cobs_soff_var = trim(cobs_soff_var)
    cobs_cord = trim(cobs_cord)
!----
    PRINT qc_global
!---- Now this global check is available for 
!     only the sherical coordinate of WOA13
    if (trim(cobs_cord) .NE. "SPHERICAL") then
       print *, "In qc_global, COBS should be SPHERICAL, Skip qc_global!"
       return
    endif   
!---- load WOA13 clomatologies of T and S (analysis)
!---- get dimensions
    call check(nf90_open(cobs_TS_file, nf90_nowrite, ncid))
    call check(nf90_inq_dimid(ncid, cobs_lon_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=cobs_nx))
    call check(nf90_inq_dimid(ncid, cobs_lat_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=cobs_ny))
    call check(nf90_inq_dimid(ncid, cobs_dep_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=cobs_nz))
    call check(nf90_inq_dimid(ncid, cobs_mon_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=cobs_nt))
!---- longitudes
    allocate(cobs_lon(cobs_nx))
    call check(nf90_inq_varid(ncid, cobs_lon_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_lon))
!---- latitudes
    allocate(cobs_lat(cobs_ny))
    call check(nf90_inq_varid(ncid, cobs_lat_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_lat))
!---- depths
    allocate(cobs_dep(cobs_nz))
    call check(nf90_inq_varid(ncid, cobs_dep_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_dep))
!---- temp
    allocate(cobs_temp(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    call check(nf90_inq_varid(ncid, cobs_temp_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_temp))
!---- salt
    allocate(cobs_salt(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    call check(nf90_inq_varid(ncid, cobs_salt_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_salt))
!---- toff
    allocate(cobs_toff(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    call check(nf90_inq_varid(ncid, cobs_toff_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_toff))
!---- soff
    allocate(cobs_soff(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    call check(nf90_inq_varid(ncid, cobs_soff_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_soff))
!----
    call check(nf90_close(ncid))

!---- set cobs_mask
    allocate(cobs_mask(cobs_nx,cobs_ny))
    cobs_mask(:,:)=1.0
    do j=1,cobs_ny 
      do i=1,cobs_nx 
        if (abs(cobs_temp(i,j,1,1) ) .GE. nbig) cobs_mask(i,j)=0.0
      enddo
    enddo
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
    logical :: ck_srch
    TYPE(profile),POINTER :: prof 
  ! TYPE(profile) :: prof 
!-------------
    ! read in from namelist
    integer :: obid_t  = 2210
    integer :: obid_pt = 2211
    integer :: obid_s  = 2220
    real :: lat_bounds(2) = (/-90,90/)
    real :: alon, alat
    integer :: bad_gb_outbound, bad_gb_noprof, bad_gb_gross
    integer :: bad_gb_gross_T, bad_gb_noprof_off_T
    integer :: bad_gb_gross_S, bad_gb_noprof_off_S
    integer :: x, y, z
    integer :: good_check_T, good_check_S
    real :: nbig = 1.0e5
    real :: hbig = 1.0e5
    real :: s, pt, v, odep, coff, cdif
    real, allocatable :: cobs_tinp(:)
    real, allocatable :: cobs_sinp(:)
    real, allocatable :: cobs_tinp_off(:) 
    real, allocatable :: cobs_sinp_off(:) 

!----
    if (.not. do_qc_global) then
       print *, "Skip qc_global"
       obs_out=obs_in
       return
    endif
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
     ! prof = obs_in%get(i)
!--- 
       if (allocated(cobs_tinp)) deallocate(cobs_tinp)
       allocate(cobs_tinp(size(prof%depth)))
       if (allocated(cobs_tinp_off)) deallocate(cobs_tinp_off)
       allocate(cobs_tinp_off(size(prof%depth)))
       if (allocated(cobs_sinp)) deallocate(cobs_sinp)
       allocate(cobs_sinp(size(prof%depth)))
       if (allocated(cobs_sinp_off)) deallocate(cobs_sinp_off)
       allocate(cobs_sinp_off(size(prof%depth)))
       !--- find nearest ocean point in cobs grid
       alon = prof%lon
       alat = prof%lat
       mdt = prof%date/100
       mon = mod(mdt,100)
       !---> Assume one of two case
       !---> obs(-180 - 180) cobs(0 - 360) : WOA09
       !---> obs(-180 - 180) cobs(-180 - 180) : WOA13
       call sphgrid_lalo2xy(alon, alat, cobs_dx, cobs_dy, cobs_dx, x, y, ck_srch)
       if (ck_srch) then
          good_check_T = 1
          good_check_S = 1
          !--- temp
          !--- find the bottom depth of cobs point
          do btma = cobs_nz,1,-1
             if (abs(cobs_temp(x,y,btma,mon)) <= nbig) EXIT
          enddo 
          do btmc = cobs_nz,1,-1
             if (abs(cobs_salt(x,y,btmc,mon)) <= nbig) EXIT
          enddo 
          !--- 
          if (btma <= 1 .and. btmc <= 1) then
             bad_gb_noprof = bad_gb_noprof + 1
             prof%tag = 52
             CALL obs_rej%push_back(prof)
             CYCLE global
          endif
          !--- Interpolate to obs depth
          !--- obs T is in-situ T
          !-- TEMP check
          if (btma > 1 .and.  size(prof%temp) .ne. 0) then
             cobs_tspl = cspline(cobs_dep(:btma), cobs_temp(x,y,:btma,mon))
             cobs_tinp = cobs_tspl%interp(prof%depth, check=.true.)
             !--- toff
             !--- check bottom of off-set data
             do btmb = cobs_nz,1,-1
                if (abs(cobs_toff(x,y,btmb,mon)) <= nbig) EXIT
             enddo
             !--- 
             if (btmb <= 1) then
                bad_gb_noprof_off_T = bad_gb_noprof_off_T + 1
                prof%tag = 53
                CALL obs_rej%push_back(prof)
                CYCLE global
             endif
             !--- Interpolate to obs depth
             cobs_tspl_off = cspline(cobs_dep(:btmb), cobs_toff(x,y,:btmb,mon))
             cobs_tinp_off = cobs_tspl_off%interp(prof%depth, check=.true.)
             !==> gross check, 
             vtemp : do k = 1, size(prof%depth)
                cdif = abs(prof%temp(k)-cobs_tinp(k))
                coff = max(cobs_gros_min_T, &
                           min(cobs_gros_max_T,cobs_gros_sdv_T*cobs_tinp_off(k))) 
                if (cdif > coff) then
                   bad_gb_gross_T = bad_gb_gross_T + 1
                   prof%tag = 51
                   CALL obs_rej%push_back(prof)
                   good_check_T = 0
                   prof%temp = PROF_UNDEF
                   EXIT vtemp
                endif
             enddo vtemp ! k = 1, size(prof%depth)
          end if !(btma > 1)
          !-- SALT check
          if (btmc > 1 .and.  size(prof%salt) .ne. 0) then
             cobs_sspl = cspline(cobs_dep(:btmc), cobs_salt(x,y,:btmc,mon))
             cobs_sinp = cobs_sspl%interp(prof%depth, check=.true.)
             !--- soff
             !--- check bottom of off-set data
             do btmd = cobs_nz,1,-1
                if (abs(cobs_soff(x,y,btmd,mon)) <= nbig) EXIT
             enddo
             !--- 
             if (btmd <= 1) then
                bad_gb_noprof_off_S = bad_gb_noprof_off_S + 1
                prof%tag = 54
                CALL obs_rej%push_back(prof)
                CYCLE global
             endif
             !--- Interpolate to obs depth
             cobs_sspl_off = cspline(cobs_dep(:btmd), cobs_soff(x,y,:btmd,mon))
             cobs_sinp_off = cobs_sspl_off%interp(prof%depth, check=.true.)
             !==> gross check, 
             vsalt : do k = 1, size(prof%depth)
                cdif = abs(prof%salt(k)-cobs_sinp(k))
                coff = max(cobs_gros_min_S, &
                           min(cobs_gros_max_S,cobs_gros_sdv_S*cobs_sinp_off(k))) 
                if (cdif > coff) then
                   bad_gb_gross_S = bad_gb_gross_S + 1
                   prof%tag = 55
                   CALL obs_rej%push_back(prof)
                   good_check_S = 0
                   prof%salt = PROF_UNDEF
                   EXIT vsalt
                endif ! (cdif > coff)
             enddo vsalt ! k = 1, size(prof%depth)
          end if !(btmc > 1)
          !--- keep T or S if good, and reject if both bad.
          !--- rej file of rejected T or S can be bigger than real rejected profile of both bad. 
          if (good_check_T + good_check_S == 0) then
             bad_gb_gross = bad_gb_gross + 1
             prof%tag = 56
             CALL obs_rej%push_back(prof)
             CYCLE global
          endif
          !-- dbg
         !PRINT *, 'IN qc_global , i= ',  &
         !      prof%date
         !if (mod(i,5000) == 0) PRINT *, 'IN qc_global , i= ', &
         !    size(cobs_tinp), size(prof%temp),btma,btmb,btmc,btmd, &
         !    prof%id,prof%lon,prof%lat,prof%date,prof%tag, &
         ! !  cobs_tinp, prof%temp, cobs_temp(x,y,:btma,mon), &
         ! !  cobs_temp(x,y,:btma,mon), cobs_toff(x,y,:btmb,mon), &
         ! !  cobs_tinp_off
         !    cobs_sinp, prof%salt, cobs_salt(x,y,:btmc,mon), &
         !    cobs_soff(x,y,:btmd,mon), cobs_sinp_off

       else ! (ck_srch)
          bad_gb_outbound = bad_gb_outbound + 1
          prof%tag = 50
          CALL obs_rej%push_back(prof)
          CYCLE global
       endif ! (ck_srch)
         
       CALL obs_out%push_back(prof)

       deallocate(cobs_tinp)
       deallocate(cobs_tinp_off)
       deallocate(cobs_sinp)
       deallocate(cobs_sinp_off)

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
   real, intent(in) :: alon, alat, cobs_dx, cobs_dy, cobs_x0
   integer, intent(inout) :: ix,jy
   logical, intent(inout) :: srch
   real :: plon, plat, pxmd, pymd
   integer :: nx, ny, n, is, ie, js, je, i, j
   !-- Find nearest grid of cobs (i and j), assumed to the shperical coordinate
   !-- add 180.0 and 90.0, to make mapping from lat-lon to grid of shperical coord.
   !-- (both longitude and latitude of obs turn to positive (now -180, -90))
   if (cobs_x0 < 0) then
      !-- (-180 - 180) -> (-180 - 180)
      plon = alon + 180.0 + 0.5*cobs_dx
   else ! (cobs_x0 < 0)
      !-- (-180 - 180)  -> (0 - 360)
      if (alon < 0) then
         plon = alon + 360.0 + 0.5*cobs_dx
      else
         plon = alon + 0.5*cobs_dx
      endif
   endif ! (cobs_x0 < 0)
   !--
   plat = alat + 90.0 + 0.5*cobs_dy
   pxmd = amod(plon,cobs_dx)
   pymd = amod(plat,cobs_dy)
   nx = int(plon/cobs_dx)
   ny = int(plat/cobs_dy)
   if (pxmd >= 0.5*cobs_dx) nx=nx+1
   if (pymd >= 0.5*cobs_dy) ny=ny+1
   !--- serching the nearest ocean grid in cobs
   if (cobs_mask(nx,ny) == 1) then
      ix = nx
      jy = ny
      srch = .TRUE.
   else ! (cobs_mask(nx,ny) == 1)
      !-- serching inner loop
      inner1 : do n = 1, cobs_fmax
         is = nx - n
         ie = nx + n
         js = ny - n
         je = ny + n
         !-- check border
         if (is <= 1) is =1
         if (js <= 1) js =1
         if (ie >= cobs_nx) ie = cobs_nx
         if (je >= cobs_ny) je = cobs_ny
         do j = js, je
            do i= is, ie
               if (cobs_mask(i,j) == 1) then
                  ix = i
                  jy = j
                  srch = .TRUE.
                  CYCLE inner1
                endif ! (cobs_mask(i,j) == 1)
             enddo ! i= is, ie
          enddo ! j = js, je
          !-- no nearest ocean point
          ix=9999
          jy=9999
          srch = .FALSE.
      end do inner1 ! n = 1, cobs_fmax     
   endif ! (cobs_mask(nx,ny) == 1)

 END SUBROUTINE sphgrid_lalo2xy

!---------------------------

  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check


  !=============================================================================

END MODULE qc_global_mod
