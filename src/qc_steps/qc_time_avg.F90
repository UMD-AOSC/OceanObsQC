!===============================================================================
!> Time averaging of profiles
!-------------------------------------------------------------------------------
MODULE qc_time_avg_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_time_avg
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE         :: init  => qc_step_init
     PROCEDURE         :: check => qc_step_check
  END TYPE qc_time_avg
  !=============================================================================


  ! parameters to be read in from the namelist
  REAL :: max_dist = 50e3 !< max horizontal distance for averaging to occur (meters)

  REAL :: time_bin = 24    !< size of time bin (hours). TODO: fix this

CONTAINS


  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_time_avg"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "Time averaging of nearby profiles from same platform"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only called once, even if the qc_step_check
  !! subroutine is called multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(self, nmlfile)
    CLASS(qc_time_avg) :: self
    INTEGER, INTENT(in) :: nmlfile

    !NAMELIST /qc_time_avg/ var1, var2
    !READ(nmlfile, qc_time_avg)
    !PRINT qc_time_avg

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
    CLASS(qc_time_avg) :: self
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    REAL, PARAMETER :: re = 6371.3d3 !< radius of earth (meters)
    REAL, PARAMETER :: deg2rad = 4.0*ATAN(1.0)/180.0!< multiply by to convert degrees to radians

    type(profile), TARGET :: prof
    TYPE(profile), POINTER :: prof1, prof2
    LOGICAL, ALLOCATABLE :: prof_valid (:)
    INTEGER, ALLOCATABLE :: prof_avg(:)
    INTEGER :: prof_avg_num

    REAL :: clat, slat, hz_dist, t_dist

    INTEGER :: i,j
    INTEGER :: rm_count

    rm_count = 0

    ALLOCATE(prof_valid(obs_in%SIZE()))
    prof_valid = .TRUE.
    ALLOCATE(prof_avg(obs_in%SIZE()))
    prof_avg_num = 0

    outer: DO i=1, obs_in%SIZE()
       ! if this profile has already been used in the averaging of a previous
       ! profile, skip it
       IF(.NOT. prof_valid(i)) CYCLE outer

       prof1 => obs_in%of(i)

       prof_avg_num = 1
       prof_avg(prof_avg_num) = i
       clat = COS(prof1%lat * deg2rad)
       slat = SIN(prof1%lat * deg2rad)

       inner: DO j= i+1, obs_in%SIZE()
          IF(.NOT. prof_valid(j)) CYCLE inner

          prof2 => obs_in%of(j)

          ! make sure same profile ID
          ! TODO, don't average if a generic '0' or SHIP id
          IF (prof1%id /= prof2%id) CYCLE inner

          ! calc distance between points
          hz_dist = re * ACOS(MIN(&
               slat*SIN(prof2%lat*deg2rad) + &
               clat*COS(prof2%lat*deg2rad)*COS((prof2%lon-prof1%lon)*deg2rad),&
               1.0))

          ! temporal distance
          t_dist = deltaDates(prof1%date, prof2%date)*24 - 12 + prof2%hour

          IF (hz_dist < max_dist .AND. ABS(t_dist) <= 12) THEN
             prof_avg_num = prof_avg_num + 1
             prof_avg(prof_avg_num) = j
          END IF
       END DO inner

       rm_count = rm_count + prof_avg_num - 1
       IF(prof_avg_num > 1) THEN
          ! mark all the profiles listed so that they aren't used again
          DO j=1,prof_avg_num
             prof_valid(prof_avg(j)) = .FALSE.
          END DO

          prof = average(obs_in, prof_avg(1:prof_avg_num))
          prof1 => prof
       END IF

       CALL obs_out%push_back(prof1)

    END DO outer

    DEALLOCATE(prof_valid, prof_avg)
    CALL print_rej_count(rm_count, 'profiles removed for temporal/hz averaging',0)

  END SUBROUTINE qc_step_check
  !=============================================================================



  !=============================================================================
  FUNCTION average(obs, obs_idx) RESULT(prof)
    use set_real_mod
    use running_stats_mod

    TYPE(vec_profile), INTENT(in)    :: obs
    INTEGER, intent(in) :: obs_idx(:)
    TYPE(profile) :: prof

    TYPE(profile), POINTER :: prof_ptr
    TYPE(set_real) :: depths_set
    TYPE(set_real_itr) :: depths_itr

    real :: r
    real, allocatable :: depths(:)
    type(running_stats), allocatable :: stats_t(:), stats_s(:)
    type(running_stats) :: stats_lat, stats_lon, stats_hr
    integer :: i, j, idx

    ! determine the cumulative set of observation levels
    !------------------------------------------------------------

    ! create a set containing all the unique depths
    depths_set = set_real()
    do i = 1, size(obs_idx)  ! for each profile to be averaged...
       prof_ptr => obs%of(obs_idx(i))

       ! TODO: consider rounding the depths?

       do j = 1, size(prof_ptr%depth) ! for each depth in the profile
          call depths_set%insert(prof_ptr%depth(j))
       end do
    end do

    
    ! get the final list of depths from the set
    ! TODO possibily thin the set, if obs aren't using standard levels?
    allocate(depths(depths_set%size()))
    depths_itr = depths_set%begin()
    i = 1
    do while(depths_itr%good())
       depths(i) = depths_itr%value()
       i = i + 1
       call depths_itr%next()
    end do



    ! for each level, calculate the number, mean, stddev
    ! also average the other variables (lat,lon,time)
    allocate(stats_t(size(depths)))
    allocate(stats_s(size(depths)))
    do i = 1, size(obs_idx)
       prof_ptr => obs%of(obs_idx(i))

       CALL stats_lat%add(real(prof_ptr%lat))
       CALL stats_lon%add(real(prof_ptr%lon))
       CALL stats_hr%add(prof_ptr%hour)

       idx=1
       do j = 1, size(prof_ptr%depth)
          ! determine the next closest target depth level
          idx = findClosest(prof_ptr%depth(j), depths(idx:))+idx-1

          ! add value to the running stats
          IF(j <= SIZE(prof_ptr%temp) .AND. prof_ptr%temp(j) /= PROF_UNDEF) THEN
             call stats_t(idx)%add(prof_ptr%temp(j))
          END IF
          IF(j <= SIZE(prof_ptr%salt) .AND. prof_ptr%salt(j) /= PROF_UNDEF) THEN
             call stats_s(idx)%add(prof_ptr%salt(j))
          END IF
          
       end do
    end do

    
    ! TODO: rerun, not adding any profile levels that fall outside desired variance
    ! TODO: remove levels that have insufficent number of obs compared with other levels
    ! TODO: remove levels that have stddev too large

    ! generate final averaged profile
    prof%id  = prof_ptr%id
    prof%lat = stats_lat%mean()
    prof%lon = stats_lon%mean()
    prof%date = prof_ptr%date
    prof%hour = stats_hr%mean()
    prof%plat = prof_ptr%plat
    allocate(prof%depth(size(depths)))
    allocate(prof%salt(size(depths)))
    allocate(prof%temp(size(depths)))
    prof%depth=depths
    prof%salt=PROF_UNDEF
    prof%temp=PROF_UNDEF
    do i=1,size(depths)
       IF(stats_t(i)%count > 0) prof%temp(i) = stats_t(i)%mean()
       IF(stats_s(i)%count > 0) prof%salt(i) = stats_s(i)%mean()       
    end do

    ! remove salt/ temp profiles that are empty
    IF(minval(prof%temp) == PROF_UNDEF) call prof%clear('T')
    IF(minval(prof%salt) == PROF_UNDEF) call prof%clear('S')

    ! done, cleanup
    deallocate(stats_t)
    deallocate(stats_s)
    deallocate(depths)

  END FUNCTION average
  !=============================================================================


  
  PURE FUNCTION findclosest(val, list) RESULT(res)
    REAL, INTENT(IN) :: val
    REAL, INTENT(IN) :: list(:)
    INTEGER :: res

    REAL :: dist, dist2
    INTEGER :: i
    
    res = -1
    dist = 1e36

    DO i = 1, size(list)
       dist2 = abs(val - list(i))
       if(dist2 < dist) THEN
          dist = dist2
       ELSE
          res = i-1
          return
       END IF       
    end DO
    res = size(list)    
  END FUNCTION FINDCLOSEST
  


  !=============================================================================
  !> returns the number of days between two dates
  !> @param date1  first date, in YYYYMMDD form
  !> @param date2  second date, in YYYYMMDD form
  !-----------------------------------------------------------------------------
  FUNCTION deltaDates(date1, date2) RESULT(days)
    INTEGER, INTENT(in) :: date1, date2
    INTEGER :: days

    INTEGER :: y, m, d
    INTEGER :: jd1, jd2


    ! get the julian date number of date 1
    y = INT(date1/10000)
    m = INT((date1-y*10000)/100)
    d = date1-y*10000-m*100
    jd1 = julianDate(y,m,d)

    ! get the julian date number of date 2
    y = INT(date2/10000)
    m = INT((date2-y*10000)/100)
    d = date2-y*10000-m*100
    jd2 = julianDate(y,m,d)

    days = jd2-jd1

  END FUNCTION deltaDates
  !=============================================================================




  !=============================================================================
  !> get a julian day number from gregorian date
  !-----------------------------------------------------------------------------
  PURE FUNCTION julianDate(Y,M,D) RESULT(jd)
    INTEGER, INTENT(in) :: y, m, d
    INTEGER :: jd

    jd = (1461*(y+4800+(m-14)/12))/4 + (367*(m-2-12*((m-14)/12)))/12 &
         - (3*((y+4900+(m-14)/12)/100))/4 + D - 32075
  END FUNCTION julianDate
  !=============================================================================


END MODULE qc_time_avg_mod
