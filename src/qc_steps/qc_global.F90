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
!!  * class name is  qc_global
!!  * filename is    qc_global.F90
!!  * module name is qc_global_mod
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
  character(len=1024), public :: cobs_cord = "SPHERICAL"    ! climate obs coordinate
  real, public :: cobs_dx = 0.25                            ! delta x in longitude
  real, public :: cobs_dy = 0.25                            ! delta y in latitude
  real, public :: cobs_x0 = -179.87                         ! first grid longitude
  real, public :: cobs_y0 = -89.875                         ! first grid latitude
  real, public :: cobs_gros_min_T = 1.0                     ! min temperature of the gross off set check
  real, public :: cobs_gros_max_T = 10.0                     ! min temperature of the gross off set check
  integer, public :: cobs_fmax = 2                          ! max number of serching sea grid in function sphgrid_lalo2xy
  real, public :: cobs_gros_sdv = 3.0                       ! max number of SDV range for the gross check
  integer, public :: cobs_nx                                ! x- dimension of cobs
  integer, public :: cobs_ny                                ! y- dimension of cobs
  integer, public :: cobs_nz                                ! z- dimension of cobs, layer
  integer, public :: cobs_nt                                ! t- dimension of cobs, months

  real, allocatable, public :: cobs_lon(:)         ! longitudes (degrees)
  real, allocatable, public :: cobs_lat(:)         ! latitudes (degrees)
  real, allocatable, public :: cobs_dep(:)         ! depths (meters)
  real, allocatable, public :: cobs_temp(:,:,:,:)  ! temp
  real, allocatable, public :: cobs_salt(:,:,:,:)  ! salt
  real, allocatable, public :: cobs_toff(:,:,:)    ! toff
  real, allocatable, public :: cobs_soff(:,:,:)    ! soff
  real, allocatable, public :: cobs_mask(:,:)      ! ocean mask

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
    character(len=:), allocatable :: cobs_T_file
    character(len=:), allocatable :: cobs_S_file
    character(len=:), allocatable :: cobs_TSOFF_file
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
      cobs_cord, cobs_dx, cobs_dy, cobs_x0, cobs_y0, & 
      cobs_fmax, cobs_gros_sdv, cobs_gros_min_T, cobs_gros_max_T, &
      cobs_T_file, cobs_S_file, cobs_TSOFF_file, &
      cobs_lon_dim,cobs_lat_dim,cobs_dep_dim,cobs_mon_dim, &
      cobs_lon_var,cobs_lat_var,cobs_dep_var, &
      cobs_temp_var, cobs_salt_var, cobs_toff_var, cobs_soff_var
       
    allocate(character(len=1024) :: cobs_T_file)
    allocate(character(len=1024) :: cobs_S_file)
    allocate(character(len=1024) :: cobs_TSOFF_file)
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
    cobs_T_file = trim(cobs_T_file)
    cobs_S_file = trim(cobs_S_file)
    cobs_TSOFF_file = trim(cobs_TSOFF_file)
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
    if (.not. do_qc_global) then
       print *, "Skip qc_global"
       return
    endif
!---- Now this global check is available for 
!     only the sherical coordinate of WOA13
    if (trim(cobs_cord) .NE. "SPHERICAL") then
       print *, "In qc_global, COBS should be SPHERICAL, Skip qc_global!"
       return
    endif   

    PRINT qc_global

!---- load WOA13 clomatologies of T and S (analysis)
!---- get dimensions
    call check(nf90_open(cobs_T_file, nf90_nowrite, ncid))
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
    call check(nf90_close(ncid))
!---- salt :  assume the same dimensions with temp
    call check(nf90_open(cobs_S_file, nf90_nowrite, ncid))
    allocate(cobs_salt(cobs_nx,cobs_ny,cobs_nz,cobs_nt))
    call check(nf90_inq_varid(ncid, cobs_salt_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_salt))
    call check(nf90_close(ncid))
!---- off-set fromWOA13 :  assume the same dimensions with temp except time
!---- monthly is better, now it is annual
!---- toff
    call check(nf90_open(cobs_TSOFF_file, nf90_nowrite, ncid))
    allocate(cobs_toff(cobs_nx,cobs_ny,cobs_nz))
    call check(nf90_inq_varid(ncid, cobs_toff_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_toff))
!---- soff
    allocate(cobs_soff(cobs_nx,cobs_ny,cobs_nz))
    call check(nf90_inq_varid(ncid, cobs_soff_var, vid))
    call check(nf90_get_var(ncid, vid, cobs_soff))
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

    INTEGER :: i, j, k, btma, btmb, btmc, mon, mdt
    logical :: ck_srch
    TYPE(profile) :: prof 
!-------------
    ! read in from namelist
    integer :: obid_t  = 2210
    integer :: obid_pt = 2211
    integer :: obid_s  = 2220
    real :: lat_bounds(2) = (/-90,90/)
    real :: alon, alat
    integer :: bad_gb_outbound, bad_gb_gross
    integer :: x, y, z
    real :: nbig = 1.0e5
    real :: s, pt, v, odep, coff, cdif
    real, allocatable :: cobs_tinp(:),cobs_tinp_off(:) 

!-------------
    bad_gb_outbound = 0
    bad_gb_gross = 0
!-------------

!--- main loop
    global : DO i = 1, obs_in%SIZE()
       prof = obs_in%get(i)
       if (allocated(cobs_tinp)) deallocate(cobs_tinp)
       allocate(cobs_tinp(size(prof%depth)))
       if (allocated(cobs_tinp_off)) deallocate(cobs_tinp_off)
       allocate(cobs_tinp_off(size(prof%depth)))
!--- find nearest ocean point in cobs grid
       alon = prof%lon
       alat = prof%lat
       mdt = prof%date/100
       mon = mod(mdt,100)
       call sphgrid_lalo2xy(alon, alat, &
            cobs_dx, cobs_dy, cobs_x0, cobs_y0, x, y, ck_srch)
       if (ck_srch) then
          !--- temp
          !--- find the bottom depth of WOA13 point
          do btma = cobs_nz,1,-1
             if (abs(cobs_temp(x,y,btma,mon)) <= nbig) exit
          enddo 
          !--- Interpolate to obs depth
          !--- obs T is in-situ T
          cobs_tspl = cspline(cobs_dep(:btma), cobs_temp(x,y,:btma,mon))
          cobs_tinp = cobs_tspl%interp(prof%depth, check=.true.)
          !--- toff
          !--- check bottom of off-set data
          do btmb = cobs_nz,1,-1
             if (abs(cobs_toff(x,y,btmb)) <= nbig) exit
          enddo 
          !--- Interpolate to obs depth
          cobs_tspl_off = cspline(cobs_dep(:btmb), cobs_toff(x,y,:btmb))
          cobs_tinp_off = cobs_tspl_off%interp(prof%depth, check=.true.)
          !==> gross check, 
          do k = 1, size(prof%depth)
             cdif = min(cobs_gros_max_T, abs(prof%temp(k)-cobs_tinp(k)))
             coff = max(cobs_gros_min_T, cobs_gros_sdv*cobs_tinp_off(k)) 
             if (cdif > coff) then
                bad_gb_gross = bad_gb_gross + 1
                prof%hour=51.0
                CALL obs_rej%push_back(prof)
                CYCLE global
             endif
          enddo ! k = 1, size(prof%depth)
          !-- dbg
          PRINT *, 'IN qc_global , i= ',  &
                prof%id
         !if (mod(i,300) == 0) PRINT *, 'IN qc_global , i= ', &
         !    size(cobs_tinp), size(prof%temp),btma,btmb, &
         !    prof%id,prof%lon,prof%lat,prof%date,prof%hour, &
         !!    cobs_tinp, prof%temp, cobs_temp(x,y,:btma,mon), &
         !    cobs_tinp_off, cobs_toff(x,y,:btmb)

       else ! (ck_srch)
          bad_gb_outbound = bad_gb_outbound + 1
          prof%hour=50.0
          CALL obs_rej%push_back(prof)
          CYCLE global
       endif ! (ck_srch)
         
!      PRINT *, 'IN qc_global , i= ', i, prof%id, size(prof%depth),size(prof%temp),size(prof%salt)
       CALL obs_out%push_back(prof)
       deallocate(cobs_tinp)
       deallocate(cobs_tinp_off)

    END DO global ! i = 1, obs_in%SIZE()

    ! print out stats if any profiles were removed
    ! NOTE: these should be printed in the order that they were checked
    IF(bad_gb_outbound > 0)&
         PRINT '(I8,A)', bad_gb_outbound, ' profiles removed for no near ocean point for Clim Obs in qc_global, h50'
    IF(bad_gb_gross > 0)&
         PRINT '(I8,A)', bad_gb_gross, ' profiles removed for out of gross check in qc_global, h51'


!---- check cobs
!     print *, cobs_dep(:),cobs_temp(720,360,:,1),cobs_salt(720,360,:,1), &
!            & cobs_toff(720,360,:),cobs_soff(720,360,:)
!--- 

  END SUBROUTINE qc_step_check

!---------------------------

 SUBROUTINE sphgrid_lalo2xy &
      (alon, alat, cobs_dx, cobs_dy, cobs_x0, cobs_y0, ix,jy,srch)
   real, intent(in) :: alon, alat, cobs_dx, cobs_dy, cobs_x0, cobs_y0
   integer, intent(inout) :: ix,jy
   logical, intent(inout) :: srch
   real :: plon, plat, pxmd, pymd
   integer :: nx, ny, n, is, ie, js, je, i, j
   !-- Find nearest grid of cobs (i and j), assumed to the shperical coordinate
   !-- add 180.0 and 90.0, to make both longitude and latitude positive (now -180, -90)
   plon = alon + 180.0 + 0.5*cobs_dx
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
                  cycle inner1
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
