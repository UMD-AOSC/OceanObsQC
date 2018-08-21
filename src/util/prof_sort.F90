!===============================================================================
!> Functions for sorting lists of profiles
!===============================================================================
MODULE prof_sort_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: profSort

  INTEGER, PUBLIC, PARAMETER :: PROF_SORT_ID = 1 !< sort by platform ID
  INTEGER, PUBLIC, PARAMETER :: PROF_SORT_DATETIME = 2 !< sort by datetime


CONTAINS


  !=============================================================================
  !> sorts a list of profiles. The original list is not modified, however, an
  !! array of indexes, which itself is sorted, is created.
  !! @param profs  the vector of profiles to sort
  !! @param sortedIdx  the resulting index array that is sorted.
  !!                    E.g. profs%get(sortedIdx(i)) will be sorted for sequential
  !!                    increasing values of i
  !! @param sortby  the method to sort by. from the PROF_SORT_* values
  !!                defined above.
  !-----------------------------------------------------------------------------
  SUBROUTINE profSort(profs, sortedIdx, sortby)
    TYPE(vec_profile), INTENT(in) :: profs
    INTEGER, ALLOCATABLE, INTENT(out) :: sortedIdx(:)
    INTEGER, INTENT(in) :: sortby

    INTEGER, PARAMETER :: M = 7 !size of subarray before insertion sort takes over
    INTEGER, PARAMETER :: nstack = 64 ! job stack size
    INTEGER :: istack(nstack)

    INTEGER :: i, j, k, ir, il, jstack, a

    ! allocate initial index array
    IF (.NOT. ALLOCATED(sortedIdx)) THEN
       ALLOCATE(sortedIdx(profs%SIZE()))
       DO i=1,SIZE(sortedIdx)
          sortedIdx(i) = i
       END DO
    END IF

    ir = SIZE(sortedIdx)
    il = 1
    jstack=0
    outer: DO
       IF(ir-il < M) THEN
          ! Do insertion sort on small segments
          do_j: DO j=il+1, ir
             a=sortedIdx(j)
             do_i: DO i=j-1,il,-1
                IF (.NOT. compG(sortedIdx(i),a)) EXIT do_i
                sortedIdx(i+1) = sortedIdx(i)
             END DO do_i
             sortedIdx(i+1) = a
          END DO do_j

          IF (jstack <= 0) EXIT outer

          ir = istack(jstack)
          il = istack(jstack-1)
          jstack = jstack - 2

       ELSE
          ! quick sort
          ! find the partitioning element, as median of left right center elements
          k = (il+ir) / 2
          CALL swapIdx(k, il+1)
          IF( compG(sortedIdx(il), sortedIdx(ir)))   CALL swapIdx(il, ir)
          IF( compG(sortedIdx(il+1), sortedIdx(ir))) CALL swapIdx(il+1, ir)
          IF( compG(sortedIdx(il), sortedIdx(il+1))) CALL swapIdx(il,il+1)
          i=il+1
          j=ir
          a=sortedIdx(il+1)
          inner: DO
             i=i+1
             DO WHILE(compL(sortedIdx(i),a))
                i=i+1
             END DO
             j=j-1
             DO WHILE(compG(sortedIdx(j),a))
                j=j-1
             END DO
             IF(j < i) EXIT inner
             CALL swapIdx(i,j)
          END DO inner

          sortedIdx(il+1) = sortedIdx(j)
          sortedIdx(j) = a
          jstack = jstack + 2

          IF (jstack > nstack) THEN
             PRINT *, "ERROR: sort ran out of stack space"
             STOP 1
          END IF

          IF(ir-i+1 >= j-il) THEN
             istack(jstack) = ir
             istack(jstack-1) = i
             ir = j-1
          ELSE
             istack(jstack) = j-1
             istack(jstack-1) = il
             il = i
          END IF
       END IF
    END DO outer



  CONTAINS


    !---------------------------------------------------------------------------
    !>  Returns whether profile 1 is greater than profile 2
    FUNCTION compG(idx1, idx2) RESULT(res)
      INTEGER, INTENT(in) :: idx1, idx2
      LOGICAL :: res
      TYPE(profile), POINTER :: prof1, prof2
      prof1 => profs%of(idx1)
      prof2 => profs%of(idx2)

      SELECT CASE(sortby)
      CASE(PROF_SORT_ID)
         res = prof1%id > prof2%id
      CASE(PROF_SORT_DATETIME)
         res = (prof1%date + prof1%hour/24.0) > (prof2%date + prof2%hour/24.0)
      END SELECT
    END FUNCTION compG
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !>  Returns whether profile 1 is less than profile 2
    FUNCTION compL(idx1, idx2) RESULT(res)
      INTEGER, INTENT(in) :: idx1, idx2
      LOGICAL :: res
      TYPE(profile), POINTER :: prof1, prof2
      prof1 => profs%of(idx1)
      prof2 => profs%of(idx2)

      SELECT CASE(sortby)
      CASE(PROF_SORT_ID)
         res = prof1%id < prof2%id
      CASE(PROF_SORT_DATETIME)
         res = (prof1%date + prof1%hour/24.0) < (prof2%date + prof2%hour/24.0)
      END SELECT
    END FUNCTION compL
    !---------------------------------------------------------------------------


    !---------------------------------------------------------------------------
    !> swap the values of the two indexes
    SUBROUTINE swapIdx( si,sj)
      INTEGER, INTENT(in) :: si, sj
      INTEGER :: st
      st = sortedIdx(si)
      sortedIdx(si)=sortedIdx(sj)
      sortedIdx(sj) = st
    END SUBROUTINE swapIdx
    !---------------------------------------------------------------------------

  END SUBROUTINE profSort
  !=============================================================================




END MODULE prof_sort_mod
