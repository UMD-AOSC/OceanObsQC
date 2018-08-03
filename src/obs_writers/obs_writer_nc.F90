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

    TYPE(profile) :: prf
    INTEGER :: obs_count, obs_offset, obs_len, i

    REAL    :: prf_lat(obs%SIZE())
    REAL    :: prf_lon(obs%SIZE())
    INTEGER :: prf_type(obs%SIZE())
    REAL    :: prf_hr(obs%SIZE())
    INTEGER :: prf_obsidx(obs%SIZE())
    INTEGER :: prf_obslen(obs%SIZE())

    REAL, ALLOCATABLE :: obs_depth(:)
    REAL, ALLOCATABLE :: obs_val(:)


    ! go through once and count the number of observations that will be produced
    obs_count = 0
    DO i=1,obs%SIZE()
       prf = obs%get(i)
       obs_count = obs_count + SIZE(prf%temp)
    END DO

    ! now, fill in the prf_ and obs_ arrays
    ALLOCATE(obs_depth(obs_count))
    ALLOCATE(obs_val(obs_count))
    obs_offset = 1
    DO i=1,obs%SIZE()
       prf = obs%get(i)
       prf_lat(i) = prf%lat
       prf_lon(i) = prf%lon
       prf_hr(i)  = 0.0
       prf_type(i) = 1
       prf_obsidx(i) = obs_offset
       obs_len = SIZE(prf%temp)
       prf_obslen(i) = obs_len
       obs_depth(obs_offset:obs_offset+obs_len) = prf%depth
       obs_val(obs_offset:obs_offset+obs_len) = prf%salt
       obs_offset = obs_offset + obs_len
    END DO

    ! output file definition
    CALL check(nf90_create(filename, NF90_NOCLOBBER, ncid))

    CALL check(nf90_def_dim(ncid, "prfs", INT(obs%SIZE()), d_prfs))
    CALL check(nf90_def_dim(ncid, "obs", obs_count, d_obs))

    CALL check(nf90_def_var(ncid, "prf_type",   nf90_short, d_prfs, vid))
    CALL check(nf90_def_var(ncid, "prf_lat",    nf90_real,  d_prfs, vid))
    CALL check(nf90_def_var(ncid, "prf_lon",    nf90_real,  d_prfs, vid))
    CALL check(nf90_def_var(ncid, "prf_hr",     nf90_real,  d_prfs, vid))
    CALL check(nf90_def_var(ncid, "prf_obsidx", nf90_int,   d_prfs, vid))
    CALL check(nf90_def_var(ncid, "prf_obslen", nf90_int,   d_prfs, vid))

    CALL check(nf90_def_var(ncid, "obs_depth",  nf90_real,  d_obs,  vid))
    CALL check(nf90_def_var(ncid, "obs_val",    nf90_real,  d_obs,  vid))

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
