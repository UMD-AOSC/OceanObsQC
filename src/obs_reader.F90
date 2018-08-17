MODULE obs_reader_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE


  !=============================================================================
  !> Abstract class from which all observation readers are based.
  !> Each class derrived from this must have a unique "name"
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT:: obs_reader
   CONTAINS
     ! procedures defined by this abstract class
     PROCEDURE :: get => obs_reader_get

     ! procedures that must be defined by the subclass
     PROCEDURE(I_reader_getstr), NOPASS, DEFERRED :: name
     PROCEDURE(I_reader_read), PRIVATE,  DEFERRED :: obs_read
  END TYPE obs_reader

  ABSTRACT INTERFACE
     FUNCTION I_reader_getstr()
       CHARACTER(:), ALLOCATABLE :: I_reader_getstr
     END FUNCTION I_reader_getstr

     SUBROUTINE I_reader_read(self, filename, obs)
       IMPORT obs_reader, vec_profile
       CLASS(obs_reader) :: self
       CHARACTER(len=*),  INTENT(in)    :: filename
       TYPE(vec_profile), INTENT(inout) :: obs
     END SUBROUTINE I_reader_read
  END INTERFACE
  !=============================================================================


  !=============================================================================
  !> Simple wrapper for the obs_reader type so that it can be placed
  !> in a vector object
  !-----------------------------------------------------------------------------
  TYPE, PUBLIC :: obs_reader_ptr
     CLASS(obs_reader), POINTER :: p
  END TYPE obs_reader_ptr
  !=============================================================================



CONTAINS



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE obs_reader_get(self, filename, start_date, end_date, obs)
    CLASS(obs_reader), INTENT(in)  :: self
    CHARACTER(len=*),  INTENT(in)  :: filename
    INTEGER,           INTENT(in)  :: start_date
    INTEGER,           INTENT(in)  :: end_date
    TYPE(vec_profile), INTENT(out) :: obs


    TYPE(vec_profile) :: obs_in
    TYPE(profile) :: ob
    INTEGER :: i, j, k
    LOGICAL :: vSalt, vTemp
    REAL, ALLOCATABLE :: tmp_r(:)


    ! read in profiles from the file
    CALL self%obs_read(filename, obs_in)

    ! do some quick checks to make sure the initial data looks okay.
    ! mainly this will just removed undefined values
    !--------------------------------------------------------------------------
    ! for each profile
    DO i=1,obs_in%SIZE()
       ob = obs_in%get(i)

       ! dont use if not within desired date range
       IF(ob%date < start_date .OR. ob%date > end_date) CYCLE

       ! if no valid temperature, remove entire array
       IF(MAXVAL(ob%temp) == MINVAL(ob%temp) .AND. ob%temp(1) == PROF_UNDEF) THEN
          DEALLOCATE(ob%temp)
          ALLOCATE(ob%temp(0))
       END  IF


       ! if no valid salt, remove entire array
       IF(MAXVAL(ob%salt) == MINVAL(ob%salt) .AND. ob%salt(1) == PROF_UNDEF) THEN
          DEALLOCATE(ob%salt)
          ALLOCATE(ob%salt(0))
       END  IF


       ! remove levels that have undefined depth, or no values
       j=0
       DO k=1,SIZE(ob%depth)
          ! is the salinity valid at this leve?
          vSalt = SIZE(ob%salt) >= k 
          if (vSalt) vSalt =  ob%salt(k) < PROF_UNDEF

          ! is the temperature valid at this level?
          vTemp = SIZE(ob%temp) >= k
          if (vTemp) vTemp =  ob%temp(k) < PROF_UNDEF

          ! if valid temp or salinity..AND valid depth
          IF ( (vSalt .OR. vTemp) .AND. ob%depth(k) < PROF_UNDEF) THEN
             j = j + 1
             ob%depth(j) =ob%depth(k)
             IF (SIZE(ob%salt) >= k) ob%salt(j) =ob%salt(k)
             IF (SIZE(ob%temp) >= k) ob%temp(j) =ob%temp(k)
          END IF
       END DO

       IF (j /= SIZE(ob%depth)) THEN
          ! levels were removed... adjust array length
          ALLOCATE(tmp_r(j))

          tmp_r = ob%depth(1:j)
          DEALLOCATE(ob%depth)
          ALLOCATE(ob%depth(j))
          ob%depth = tmp_r

          IF(SIZE(ob%temp) > 0) THEN
             tmp_r = ob%temp(1:j)
             DEALLOCATE(ob%temp)
             ALLOCATE(ob%temp(j))
             ob%temp = tmp_r
          END IF

          IF(SIZE(ob%salt) > 0) THEN
             tmp_r = ob%salt(1:j)
             DEALLOCATE(ob%salt)
             ALLOCATE(ob%salt(j))
             ob%salt = tmp_r
          END IF

          DEALLOCATE(tmp_r)
       END IF

       ! if no salt AND no temp, don't use this profile
       IF(SIZE(ob%salt) == 0 .AND. SIZE(ob%temp) == 0) CYCLE

       ! done, use this ob
       CALL obs%push_back(ob)

    END DO

  END SUBROUTINE obs_reader_get
  !=============================================================================


END MODULE obs_reader_mod
!===============================================================================




!===============================================================================
!> Templated vector object for holding obs_reader_ptr types
!-------------------------------------------------------------------------------
MODULE vec_obs_reader_mod
  USE obs_reader_mod
#define _type type(obs_reader_ptr)
#define _vector vec_obs_reader
#include "templates/vector.inc"
END MODULE vec_obs_reader_mod
!===============================================================================
