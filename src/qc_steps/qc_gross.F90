!===============================================================================
!>
!-------------------------------------------------------------------------------
MODULE qc_gross_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod
  USE, INTRINSIC :: ieee_arithmetic
  
  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_gross
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE         :: init  => qc_step_init
     PROCEDURE         :: check => qc_step_check
  END TYPE qc_gross
  !=============================================================================


  ! parameters to be read in from the namelist
  LOGICAL :: remove_zero_latlon = .TRUE.
  REAL :: t_min = -100
  REAL :: t_max = 100
  REAL :: s_min = 0
  REAL :: s_max = 100



CONTAINS



  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_gross"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "gross bounds checking on values"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only called once, even if the qc_step_check
  !! subroutine is called multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(self, nmlfile)
    CLASS(qc_gross) :: self
    INTEGER, INTENT(in) :: nmlfile

    NAMELIST /qc_gross/ &
         remove_zero_latlon, t_min, t_max, s_min, s_max
    READ(nmlfile, qc_gross)
    PRINT qc_gross

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
    CLASS(qc_gross) :: self
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    INTEGER :: i, k
    TYPE(profile), POINTER :: prof
    TYPE(profile) :: prof_rej

    INTEGER :: bad_zero_latlon, bad_t_max, bad_t_min, bad_s_max, &
         bad_s_min, bad_lat, bad_lon, bad_date, bad_hour, bad_t_nan, bad_s_nan
    INTEGER :: tag_zero_latlon, tag_t_max, tag_t_min, tag_s_max, &
         tag_s_min, tag_lat, tag_lon, tag_date, tag_hour, tag_t_nan, tag_s_nan
    LOGICAL :: keep

    INTEGER :: yr, mn, dy
    REAL :: hr
    
    ! initialize bad obs counts
    bad_zero_latlon = 0
    bad_lat = 0
    bad_lon = 0
    bad_date = 0
    bad_hour = 0
    bad_t_max = 0
    bad_t_min = 0
    bad_t_nan = 0
    bad_s_max = 0
    bad_s_min = 0
    bad_s_nan = 0

    ! initialize error tag values
    tag_lat         = self%err_base + 1
    tag_lon         = self%err_base + 2
    tag_zero_latlon = self%err_base + 3
    tag_date        = self%err_base + 4
    tag_hour        = self%err_base + 5
    tag_t_max       = self%err_base + 6
    tag_t_min       = self%err_base + 7
    tag_t_nan       = self%err_base + 8
    tag_s_max       = self%err_base + 9
    tag_s_min       = self%err_base + 10
    tag_s_nan       = self%err_base + 11

    ! check each profile
    do_loop: DO i = 1, obs_in%SIZE()
       prof => obs_in%of(i)


       ! check for bad latitudes
       IF (prof%lat > 90.0 .OR. prof%lat < -90.0) THEN
          bad_lat = bad_lat + 1
          prof%tag = tag_lat
          CALL obs_rej%push_back(prof)
          CYCLE do_loop
       END IF


       ! check for bad longitudes
       IF (prof%lon < -360.0 .OR. prof%lon > 360.0) THEN
          bad_lon = bad_lon + 1
          prof%tag = tag_lon
          CALL obs_rej%push_back(prof)
          CYCLE do_loop
       END IF


       ! check for lat=0 lon=0
       IF (remove_zero_latlon) THEN
          IF (prof%lat == 0.0 .AND. prof%lon == 0.0) THEN
             bad_zero_latlon = bad_zero_latlon + 1
             prof%tag = tag_zero_latlon
             CALL obs_rej%push_back(prof)
             CYCLE do_loop
          END IF
       END IF

       
       ! check for bad date
       yr = prof%date / 10000
       mn = MOD((prof%date / 100 ), 100)
       dy = MOD(prof%date, 100)
       keep = .TRUE.
       ! gross check on dates, i'm assuming my code will no longer be used by year 2100...     
       IF (mn<1 .or. mn >12 .or. dy<=0 .or. dy >31 .or. yr<1000 .or. yr>2100) keep = .FALSE.
       ! months with 30 days
       IF ((mn==9 .or. mn==4 .or. mn==6 .or. mn==11) .and. dy > 30) keep = .FALSE.
       IF( mn == 2) THEN
          IF(mod(yr,4)==0 .and. (mod(yr,100)/=0 .or. mod(yr,400)==0)) THEN
             ! feb, during leap year
             IF ( dy > 29) keep = .FALSE.
          ELSE
             ! feb, during non-leap year
             IF ( dy > 28) keep = .FALSE.
          END IF
       END IF
       IF (.NOT. keep) THEN
          ! otherwise, bad date
          bad_date = bad_date + 1
          prof%tag = tag_date
          CALL obs_rej%push_back(prof)
          CYCLE do_loop
       END IF


       ! check for a bad hour
       IF(prof%hour == PROF_UNDEF) prof%hour = 12.0       
       IF (prof%hour < 0 .OR. prof%hour > 24.0) THEN
          bad_hour = bad_hour + 1
          prof%tag = tag_hour
          CALL obs_rej%push_back(prof)
          CYCLE do_loop
       END IF
       
       
       ! check bad temperature values
       temp_loop: DO k=1, SIZE(prof%temp)
          IF(prof%temp(k) == PROF_UNDEF) CYCLE

          keep = .FALSE.
          IF(prof%temp(k) > t_max) THEN
             bad_t_max = bad_t_max + 1
             prof_rej = prof%copy('T')
             prof_rej%tag = tag_t_max
          ELSE IF(prof%temp(k) < t_min) THEN
             bad_t_min = bad_t_min + 1
             prof_rej = prof%copy('T')
             prof_rej%tag = tag_t_min
          ELSE IF( IEEE_IS_NAN(prof%temp(k))) THEN
             bad_t_nan = bad_t_nan + 1
             prof_rej = prof%copy('T')
             prof_rej%tag = tag_t_nan
          ELSE
             keep=.TRUE.
          END IF

          IF(.NOT. keep) THEN
             CALL obs_rej%push_back(prof_rej)
             CALL prof%clear('T')
             EXIT temp_loop
          END IF
       END DO temp_loop


       ! check bad salinity values
       salt_loop: DO k=1, SIZE(prof%salt)
          IF(prof%salt(k) == PROF_UNDEF) CYCLE

          keep = .FALSE.
          IF(prof%salt(k) > s_max) THEN
             bad_s_max = bad_s_max + 1
             prof_rej = prof%copy('S')
             prof_rej%tag = tag_s_max
          ELSE IF(prof%salt(k) < s_min) THEN
             bad_s_min = bad_s_min + 1
             prof_rej = prof%copy('S')
             prof_rej%tag = tag_s_min
          ELSE IF( IEEE_IS_NAN(prof%salt(k))) THEN
             bad_s_nan = bad_s_nan + 1
             prof_rej = prof%copy('S')
             prof_rej%tag = tag_s_nan
          ELSE             
             keep=.TRUE.
          END IF

          IF(.NOT. keep) THEN
             CALL obs_rej%push_back(prof_rej)
             CALL prof%clear('S')
             EXIT salt_loop
          END IF
       END DO salt_loop


       ! make sure there is at least a valid T or S remaining
       IF(SIZE(prof%salt) == 0 .AND. SIZE(prof%temp) == 0) CYCLE do_loop


       ! if we made it this far, the profile is good
       CALL obs_out%push_back(prof)


    END DO do_loop



    ! print out stats if any profiles were removed
    CALL print_rej_count(bad_lat, 'profiles removed for bad latitude', tag_lat)
    CALL print_rej_count(bad_lon, 'profiles removed for bad longitude', tag_lon)
    CALL print_rej_count(bad_zero_latlon, 'profiles removed because lat=0.0 and lon=0.0', tag_zero_latlon)
    CALL print_rej_count(bad_date, 'profiles removed for bad date', tag_date)
    CALL print_rej_count(bad_hour, 'profiles removed for bad hour', tag_hour)
    CALL print_rej_count(bad_t_max, 'T profiles removed because T > t_max', tag_t_max)
    CALL print_rej_count(bad_t_min, 'T profiles removed because T < t_min', tag_t_min)
    CALL print_rej_count(bad_t_nan, 'T profiles removed because T == NaN',  tag_t_nan) 
    CALL print_rej_count(bad_s_max, 'S profiles removed because S > s_max', tag_s_max)
    CALL print_rej_count(bad_s_min, 'S profiles removed because S < s_min', tag_s_min)
    CALL print_rej_count(bad_s_nan, 'S profiles removed because S == NaN',  tag_s_nan) 

  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_gross_mod
