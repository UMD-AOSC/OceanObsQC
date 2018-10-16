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
     PROCEDURE(I_reader_init),   NOPASS, DEFERRED :: init
     PROCEDURE(I_reader_read),   NOPASS, PRIVATE, DEFERRED :: obs_read
  END TYPE obs_reader

  ABSTRACT INTERFACE
     FUNCTION I_reader_getstr()
       CHARACTER(:), ALLOCATABLE :: I_reader_getstr
     END FUNCTION I_reader_getstr

     SUBROUTINE I_reader_init(nmlfile)
       INTEGER, INTENT(in) :: nmlfile
     END SUBROUTINE I_reader_init

     SUBROUTINE I_reader_read(filename, obs)
       IMPORT vec_profile
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
    TYPE(profile), POINTER :: ob
    INTEGER :: i, j
    INTEGER :: rm_count(6)

    ! read in profiles from the file
    CALL self%obs_read(filename, obs_in)

    ! do some quick checks to make sure the initial data looks okay.
    ! mainly this will just remove undefined values
    !--------------------------------------------------------------------------
    ! for each profile
    rm_count = 0
    DO i=1,obs_in%SIZE()
       ob => obs_in%of(i)

       ! dont use if not within desired date range
       IF(ob%date < start_date .OR. ob%date > end_date) THEN
          rm_count(1) = rm_count(1) + 1
          CYCLE
       END IF

       ! check the levels, remove empty levels or variables
       CALL ob%check(j)

       SELECT CASE(j)
       CASE (PROF_CHECK_NO_VARS)
          ! don't add this profile to the valid obs
          rm_count(2) = rm_count(2) + 1
          CYCLE
       CASE (PROF_CHECK_NO_T)
          rm_count(3) = rm_count(3) + 1
       CASE (PROF_CHECK_NO_S)
          rm_count(4) = rm_count(4) + 1
       CASE (PROF_CHECK_RMLVL)
          rm_count(5) = rm_count(5) + 1
       END SELECT

       ! done, use this ob
       CALL obs%push_back(ob)

    END DO


    ! print out warnings if profiles were removed
    IF(rm_count(1) > 0) &
         PRINT '(A,I8,A)', "", rm_count(1), " profiles removed due to outside date range"
    IF(rm_count(2) > 0) &
         PRINT '(A,I8,A)', "", rm_count(2), " profiles removed due to no valid variables"
    IF(rm_count(3) > 0) &
         PRINT '(A,I8,A)', "", rm_count(3), " T profiles removed due to constant PROF_UNDEF"
    IF(rm_count(4) > 0) &
         PRINT '(A,I8,A)', "", rm_count(4), " S profiles removed due to constant PROF_UNDEF"
    IF(rm_count(5) > 0) &
         PRINT '(A,I8,A)', "", rm_count(5), " profiles had empty levels removed "


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
