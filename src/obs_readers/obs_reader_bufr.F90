MODULE obs_reader_bufr_mod
  USE obs_reader_mod
  USE profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_reader), PUBLIC :: obs_reader_bufr
   CONTAINS
     PROCEDURE, NOPASS :: name => bufr_get_name
     PROCEDURE         :: obs_read => bufr_read
  END TYPE obs_reader_bufr
  !=============================================================================


CONTAINS


  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION bufr_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "BUFR"
  END FUNCTION bufr_get_name
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE bufr_read(self, obs)
    CLASS(obs_reader_bufr) :: self
    TYPE(profile), ALLOCATABLE, INTENT(out) :: obs(:)

    LOGICAL :: valid
    TYPE(profile) :: ob
    INTEGER :: file, idate, iret
    CHARACTER*8 c1

    INTEGER ::cnt
    !open file
    file=90
    OPEN(unit=file, file='xx005')
    CALL openbf(file,'IN',file)
    CALL datelen(10)

    ! process each profile
    cnt = 0
    iret = 0
    DO
       CALL readmg(file, c1, idate, iret)
       IF (iret /= 0) EXIT

       DO
          CALL readsb(file, iret)
          IF (iret /= 0) EXIT

          valid = .FALSE.
          IF (c1 == "NC031001" .OR. c1 == "NC031002") THEN
             CALL process_bathytesac(file, ob, valid)
          ELSE IF(c1 == "NC031005") THEN
             CALL process_float(file, ob, valid)
          ELSE
             PRINT *, "WARN: unknown ob type: ", c1
          END IF
          IF(valid) cnt = cnt + 1
       END DO
    END DO

    PRINT *, "Found ",cnt,"profiles"
    ! all done cleanup
    CALL closbf(file)
    CLOSE(file)
  END SUBROUTINE bufr_read
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE process_bathytesac(file, ob, valid)
    INTEGER, INTENT(in) :: file
    TYPE(profile), INTENT(out) :: ob
    LOGICAL, INTENT(out) :: valid

    INTEGER :: nlv, i
    INTEGER, PARAMETER :: MXMN=10, MXLV=1000
    REAL*8  r8(MXMN, MXLV)

    CHARACTER str*(8), str2*(8)
    EQUIVALENCE (r8, str)

    PRINT *, ""
    ! datetime
    CALL UFBSEQ(file, r8, MXMN, MXLV, nlv, 'YYMMDD')
    !    PRINT *, 'YYMMDD ', r8(1:3,1)
    CALL UFBSEQ(file, r8, MXMN, MXLV, nlv, 'HHMM')
    !PRINT *, 'HHMM ', r8(1:2,1)
    CALL ufbint(file, r8, MXMN, MXLV, nlv, 'RPID')
    !PRINT *, "RPID ", nlv, str
    CALL ufbseq(file, r8, MXMN, MXLV, nlv, 'LTLONH')
    !PRINT *, "LTLONH ", r8(1:2,1)

    CALL ufbrep(file, r8, MXMN, MXLV, nlv, 'DBSS STMP SALN')
    !DO i = 1, nlv
    !PRINT *, "     ", i,r8(1:3,i)
    !END DO

    valid = .TRUE.
  END SUBROUTINE process_bathytesac
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE process_float(file, ob, valid)
    INTEGER, INTENT(in) :: file
    TYPE(profile), INTENT(out) :: ob
    LOGICAL, INTENT(out) :: valid

    INTEGER :: nlv, i
    INTEGER, PARAMETER :: MXMN=10, MXLV=1000
    REAL*8  r8(MXMN, MXLV)

    CHARACTER str*(8), str2*(8)
    EQUIVALENCE (r8, str)

    !    PRINT *, ""
    ! datetime
    CALL UFBSEQ(file, r8, MXMN, MXLV, nlv, 'YYMMDD')
    !    PRINT *, 'YYMMDD ', r8(1:3,1)
    CALL UFBSEQ(file, r8, MXMN, MXLV, nlv, 'HHMM')
    !PRINT *, 'HHMM ', r8(1:2,1)
    CALL ufbint(file, r8, MXMN, MXLV, nlv, 'WMOP')
    !PRINT *, "WMOP ", INT(r8(1,1))
    CALL ufbseq(file, r8, MXMN, MXLV, nlv, 'LTLONH')
    !    PRINT *, "LTLONH ", r8(1:2,1)

    CALL ufbrep(file, r8, MXMN, MXLV, nlv, 'WPRES SSTH SALNH')
    !PRINT *, nlv
    !DO i = 1, nlv
    !PRINT *, "     ", i,r8(1:3,i)
    !END DO

    valid=.TRUE.

  END SUBROUTINE process_float
  !=============================================================================

END MODULE obs_reader_bufr_mod
