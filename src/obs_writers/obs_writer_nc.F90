MODULE obs_writer_nc_mod
  USE obs_writer_mod
  USE profile_mod
  USE vec_profile_mod
  USE netcdf

  IMPLICIT NONE
  PRIVATE



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_writer), PUBLIC :: obs_writer_nc
   CONTAINS
     PROCEDURE, NOPASS :: name => writer_nc_get_name
     PROCEDURE         :: obs_write => writer_nc_write
  END TYPE obs_writer_nc
  !=============================================================================



CONTAINS



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION writer_nc_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "NC"
  END FUNCTION writer_nc_get_name
  !=============================================================================


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE writer_nc_write(self, filename, obs)
    CLASS(obs_writer_nc), INTENT(inout) :: self
    CHARACTER(len=*),  INTENT(in)    :: filename
    TYPE(vec_profile), INTENT(in) :: obs

    INTEGER :: ncid, d_prfs, d_obs, vid

    TYPE(profile), POINTER :: prf
    INTEGER :: obs_count, obs_offset, obs_len, i, j, p, o
    INTEGER :: prf_count

    REAL,    ALLOCATABLE :: prf_lat(:)
    REAL,    ALLOCATABLE :: prf_lon(:)
    INTEGER, ALLOCATABLE :: prf_type(:)
    REAL,    ALLOCATABLE :: prf_hr(:)
    INTEGER, ALLOCATABLE :: prf_obsidx(:)
    INTEGER, ALLOCATABLE :: prf_obslen(:)
    REAL,    ALLOCATABLE :: obs_depth(:)
    REAL,    ALLOCATABLE :: obs_val(:)


    ! go through once and count the number of observations/profiles that will be produced
    prf_count = 0
    obs_count = 0
    DO i=1,obs%SIZE()
       prf => obs%of(i)
       IF( SIZE(prf%temp) > 0) THEN
          prf_count = prf_count + 1
          obs_count = obs_count + SIZE(prf%temp)
       END IF
       IF( SIZE(prf%salt) > 0) THEN
          prf_count = prf_count + 1
          obs_count = obs_count + SIZE(prf%salt)
       END IF
    END DO


    ! now, fill in the prf_ and obs_ arrays
    ALLOCATE(prf_lat(prf_count))
    ALLOCATE(prf_lon(prf_count))
    ALLOCATE(prf_type(prf_count))
    ALLOCATE(prf_hr(prf_count))
    ALLOCATE(prf_obsidx(prf_count))
    ALLOCATE(prf_obslen(prf_count))
    ALLOCATE(obs_depth(obs_count))
    ALLOCATE(obs_val(obs_count))
    obs_offset = 1
    p = 0
    DO i=1,obs%SIZE()
       prf => obs%of(i)
       DO j=1,2
          IF (j==1 .AND. SIZE(prf%temp) == 0) CYCLE
          IF (j==2 .AND. SIZE(prf%salt) == 0) CYCLE
          p = p + 1
          prf_lat(p) = prf%lat
          prf_lon(p) = prf%lon
          prf_hr(p)  = prf%hour
          prf_type(p) = j
          prf_obsidx(p) = obs_offset

          obs_len = MERGE(SIZE(prf%temp), SIZE(prf%salt), j==1)
          prf_obslen(p) = obs_len
          obs_depth(obs_offset:obs_offset+obs_len-1) = prf%depth
          obs_val(obs_offset:obs_offset+obs_len-1) = MERGE(prf%temp, prf%salt, j==1)
          obs_offset = obs_offset + obs_len
       END DO
    END DO


    ! output file definition
    CALL check(nf90_create(filename, NF90_CLOBBER, ncid))

    CALL check(nf90_def_dim(ncid, "prfs", INT(prf_count), d_prfs))
    CALL check(nf90_def_dim(ncid, "obs", obs_count, d_obs))

    CALL check(nf90_def_var(ncid, "prf_type",   nf90_short, d_prfs, vid))
    CALL check(nf90_put_att(ncid, vid, "description", &
         "Profile type: TEMP=1, SALT=2"))

    CALL check(nf90_def_var(ncid, "prf_lat",    nf90_real,  d_prfs, vid))
    CALL check(nf90_put_att(ncid, vid, "description", "latitude"))
    CALL check(nf90_put_att(ncid, vid, "units", "degrees_east"))

    CALL check(nf90_def_var(ncid, "prf_lon",    nf90_real,  d_prfs, vid))
    CALL check(nf90_put_att(ncid, vid, "description", "longitude"))
    CALL check(nf90_put_att(ncid, vid, "units", "degrees_north"))

    CALL check(nf90_def_var(ncid, "prf_hr",     nf90_real,  d_prfs, vid))
    CALL check(nf90_put_att(ncid, vid, "description", "time"))
    CALL check(nf90_put_att(ncid, vid, "units", "hours"))

    CALL check(nf90_def_var(ncid, "prf_obsidx", nf90_int,   d_prfs, vid))
    CALL check(nf90_put_att(ncid, vid, "description", &
         "starting index (1 based array) in obs_* arrays of observations for this profile"))
    CALL check(nf90_def_var(ncid, "prf_obslen", nf90_int,   d_prfs, vid))
    CALL check(nf90_put_att(ncid, vid, "description", &
         "number of entries in obs_* arrays for this profile"))

    CALL check(nf90_def_var(ncid, "obs_depth",  nf90_real,  d_obs,  vid))
    CALL check(nf90_put_att(ncid, vid, "description", "depth of single obs_val observation"))
    CALL check(nf90_put_att(ncid, vid, "units", "meters"))

    CALL check(nf90_def_var(ncid, "obs_val",    nf90_real,  d_obs,  vid))
    CALL check(nf90_put_att(ncid, vid, "description", "observation value at obs_depth level"))
    CALL check(nf90_put_att(ncid, vid, "units", "celsius (if type=1), PSU (if type=2)"))

    CALL check(nf90_put_att(ncid, NF90_GLOBAL, "source", "NCEP observation profile quality control program"))
    CALL check(nf90_put_att(ncid, NF90_GLOBAL, "source_version", CVERSION))
    CALL check(nf90_put_att(ncid, NF90_GLOBAL, "descriptions", "Ocean profiles that have undergone quality control"//&
         " checks. Temperature and Salinity are considered separate profiles. Each profile has an entry in the prf_ variables. "//&
         " The observations within each profile are placed in the obs_ variables, with array starting index and length "//&
         " specified by prf_obsidx and prf_obslen. For example, observations for the first profile are given by "//&
         " obs_val(prf_obsidx(1) : prf_obsidx(1)+prf_obslen(1)), assuming Fortran."))

    CALL check(nf90_enddef(ncid))


    ! put values in the output file
    CALL check(nf90_inq_varid(ncid, "prf_type", vid))
    CALL check(nf90_put_var(ncid, vid, prf_type))

    CALL check(nf90_inq_varid(ncid, "prf_lat", vid))
    CALL check(nf90_put_var(ncid, vid, prf_lat))

    CALL check(nf90_inq_varid(ncid, "prf_lon", vid))
    CALL check(nf90_put_var(ncid, vid, prf_lon))

    CALL check(nf90_inq_varid(ncid, "prf_hr", vid))
    CALL check(nf90_put_var(ncid, vid, prf_hr))

    CALL check(nf90_inq_varid(ncid, "prf_lon", vid))
    CALL check(nf90_put_var(ncid, vid, prf_lon))

    CALL check(nf90_inq_varid(ncid, "prf_obsidx", vid))
    CALL check(nf90_put_var(ncid, vid, prf_obsidx))

    CALL check(nf90_inq_varid(ncid, "prf_obslen", vid))
    CALL check(nf90_put_var(ncid, vid, prf_obslen))

    CALL check(nf90_inq_varid(ncid, "obs_depth", vid))
    CALL check(nf90_put_var(ncid, vid, obs_depth))

    CALL check(nf90_inq_varid(ncid, "obs_val", vid))
    CALL check(nf90_put_var(ncid, vid, obs_val))

    ! all done, clean up
    CALL check(nf90_close(ncid))
    DEALLOCATE(obs_depth)
    DEALLOCATE(obs_val)

  END SUBROUTINE writer_nc_write
  !=============================================================================


  !=============================================================================
  SUBROUTINE check(status)
    INTEGER, INTENT(in) :: status
    IF(status /= nf90_noerr) THEN
       PRINT *, TRIM(nf90_strerror(status))
       STOP 1
    END IF
  END SUBROUTINE check
  !=============================================================================

END MODULE obs_writer_nc_mod
