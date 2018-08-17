MODULE obs_reader_wod_mod
  USE obs_reader_mod
  USE profile_mod
  USE vec_profile_mod

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(obs_reader), PUBLIC :: obs_reader_wod
   CONTAINS
     PROCEDURE, NOPASS :: name => wod_get_name
     PROCEDURE         :: obs_read => wod_read
  END TYPE obs_reader_wod
  !=============================================================================


  REAL :: WOD_UNDEF_REAL = -9.99e36
  INTEGER :: WOD_UNDEF_INT = -999.9



CONTAINS



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  FUNCTION wod_get_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "WOD"
  END FUNCTION wod_get_name
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE wod_read(self, filename, obs)
    CLASS(obs_reader_wod) :: self
    CHARACTER(len=*),  INTENT(in)    :: filename
    TYPE(vec_profile), INTENT(inout) :: obs

    LOGICAL :: valid
    INTEGER :: unit

    TYPE(profile) :: ob

    ! make sure input file exists
    INQUIRE(file=filename, exist=valid)
    IF(.NOT. valid) THEN
       PRINT *, "input file not found: ", TRIM(filename)
       STOP 1
    END IF

    ! open file
    OPEN(newunit=unit, file=filename, action='READ', status='old')
    valid = .TRUE.
    DO WHILE(valid)
       CALL read_wod_rec(unit, ob, valid)
       IF(valid) CALL obs%push_back(ob)
    END DO

    ! all done, close file
    CLOSE(unit)

  END SUBROUTINE wod_read
  !=============================================================================



  !=============================================================================
  !>
  !-----------------------------------------------------------------------------
  SUBROUTINE read_wod_rec(unit, ob, valid)
    INTEGER, INTENT(in) :: unit
    TYPE(profile), INTENT(out) :: ob
    LOGICAL, INTENT(out) :: valid

    INTEGER :: i, j, k, n

    ! TODO, is it oaky to hardcode the max record size???
    CHARACTER*1500000 :: ascii
    INTEGER :: fLen, tmp_i, pos
    REAL :: tmp_r

    REAL :: depth, val

    INTEGER :: num_lvl
    INTEGER :: prof_len, num_var, num_var_meta
    INTEGER :: num_taxa, num_taxa_entries

    INTEGER :: var_code(10)
    CHARACTER*2 :: cc
    valid = .TRUE.


    ! initialize observatino
    ob%plat = PLAT_UNKNOWN


    ! read the first line
    READ(unit, '(a80)', iostat=i) ascii
    IF (i /= 0) THEN
       valid = .FALSE.
       RETURN
    END IF


    ! --------------------------------------------------------------------------
    ! Read primary header
    ! --------------------------------------------------------------------------

    ! determine the WOD format, only continue if format is new enough
    IF(  ascii(1:1) /= 'C') THEN
       PRINT *, "ERROR: this appears to NOT be a WOD13 formatted file. "//&
            " Please download a newer file format."
       STOP 1
    END IF

    pos = 2 ! cursor position for record reading

    ! get length of profile record
    prof_len = readInt()

    ! read in the rest of the lines
    DO i = 2, CEILING(prof_len/80.0)
       READ(unit, '(a80)') ascii((i-1)*80+1 : i*80)
    END DO

    ! start parsing data
    !--------------------------------------------------------------------------

    tmp_i = readInt() ! WOD cast number

    ! platform ID is combo of country code and cruise number
    cc = ascii(pos:pos+1); pos = pos + 2 ! country code
    tmp_i = readInt() ! WOD cruise number
    WRITE (ob%id, '(A2,I0)')  cc, tmp_i

    ! year, month, day as single integer
    ob%date = &
         readInt(4)*10000 + & ! year
         readInt(2)*100 + &   ! month
         readInt(2)           ! day

    ! hour. Replace with 12Z if no hour is given (likely due to daily averaging)
    ob%hour = readReal() ! hour, fractional
    IF(ob%hour == PROF_UNDEF) ob%hour = 12.0

    ob%lat = readReal() ! latitude
    ob%lon = readReal() ! longitude

    num_lvl = readInt() ! number of vertical levels
    ALLOCATE(ob%depth(num_lvl))
    tmp_i = readInt(1); ! profile level type,  0=OBS, 1=STD

    num_var = readInt(2) ! number of variables
    DO i = 1, num_var
       var_code(i) = readInt() ! variable type code
       IF(var_code(i) == 1) THEN ! temp
          ALLOCATE(ob%temp(num_lvl))
       ELSE IF(var_code(i) == 2) THEN
          ALLOCATE(ob%salt(num_lvl))
       END IF

       tmp_i = readInt(1) ! variable qc
       num_var_meta = readInt() ! number of var meta data entries
       DO j=1, num_var_meta
          tmp_i = readInt() ! var meta type code
          tmp_r = readReal() ! var meta value
       END DO
    END DO

    IF (.NOT. ALLOCATED(ob%temp)) ALLOCATE(ob%temp(0))
    IF (.NOT. ALLOCATED(ob%salt)) ALLOCATE(ob%salt(0))
    ob%temp = PROF_UNDEF
    ob%salt = PROF_UNDEF


    ! --------------------------------------------------------------------------
    ! Read character data and PI header
    ! --------------------------------------------------------------------------
    n = readInt() ! character data length
    IF (n /= WOD_UNDEF_INT .AND. n/= 0) THEN
       DO n=1, readInt(1) ! number of entries
          SELECT CASE(readInt(1))

          CASE(1) ! cruise ID from originator
             i = readInt(2)
             !ob%id = ascii(pos:pos+i-1)
             ! shouldn't need this?? unless need ID to match with other
             ! data sources
             pos = pos + i

          CASE(2) ! cast ID from originator
             i = readInt(2)
             pos = pos + i

          CASE(3) ! PI
             DO j=1, readInt(2) !num of PI names
                i = readInt() ! var code
                i = readInt() ! bytes in name
                pos = pos + i
             END DO
          END SELECT
       END DO
    END IF


    ! --------------------------------------------------------------------------
    ! Secondary header
    ! --------------------------------------------------------------------------
    n = readInt() ! second header length
    IF (n /= WOD_UNDEF_INT .AND. n /= 0) THEN
       DO n = 1, readInt() ! number of entries
          tmp_i = readInt() ! header code
          tmp_r = readReal() ! value

          IF(tmp_i == 29) THEN ! probe type
             SELECT CASE(INT(tmp_r))
             CASE (1,2,3) ! MBT, XBT, DBT
                ob%plat = PLAT_BATHY
             CASE (10, 11) ! buoy - moored, drifting
                ob%plat = PLAT_BUOY
             CASE (9, 13) ! profiling float, animal mounted
                ob%plat = PLAT_FLOAT
             END SELECT

          END IF
       END DO
    END IF


    ! --------------------------------------------------------------------------
    ! Biological header
    ! --------------------------------------------------------------------------
    n = readInt() ! bio header length
    IF (n /= WOD_UNDEF_INT .AND. n/= 0) THEN
       ! don't bother reading this header
       pos = pos + n
    END IF


    ! --------------------------------------------------------------------------
    ! profile data
    ! --------------------------------------------------------------------------
    do_lvls: DO i=1,num_lvl
       ob%depth(i) = readReal() ! dpth
       !       IF(depth == WOD_UNDEF_REAL) CYCLE do_lvls
       pos = pos + 2
       !       tmp_i = readInt(1) ! depth err
       !       tmp_i = readInt(1) ! depth err o

       do_vars: DO j=1, num_var
          val = readReal() ! val
          IF(val == WOD_UNDEF_REAL) CYCLE do_vars
          pos = pos + 2
          IF(var_code(j) == 1) THEN
             ob%temp(i) = val
          ELSE IF(var_code(j) == 2) THEN
             ob%salt(i) = val
          END IF
          !          tmp_i = readInt(1) ! val qc
          !          tmp_i = readInt(1) ! val qc O
       END DO do_vars
    END DO do_lvls


  CONTAINS



    !===========================================================================
    !> read an integer value from the current position (pos) of the string
    !! (ascii). Length in bytes is given by nbytes. If nbytes is not given
    !! the number of bytes to use for the integer is read in from the first
    !! bytes of the string. "pos" will but updated afterward
    !---------------------------------------------------------------------------
    FUNCTION readInt(nbytes) RESULT(res)
      INTEGER :: res
      INTEGER, OPTIONAL, INTENT(in) :: nbytes
      INTEGER :: nbytes0

      IF(PRESENT(nbytes)) THEN
         nbytes0 = nbytes
      ELSE
         READ(ascii(pos:pos),*) nbytes0
         pos = pos + 1
      END IF

      IF ( nbytes0 == 0) THEN
         res = WOD_UNDEF_INT
      ELSE
         READ(ascii(pos:pos+nbytes0-1), *) res
      END IF

      pos = pos + nbytes0

    END FUNCTION readInt
    !===========================================================================



    !===========================================================================
    FUNCTION readReal() RESULT(res)
      REAL :: res
      INTEGER :: num_sig, num_total, num_prec
      INTEGER :: val_i

      IF (ascii(pos:pos) == '-')  THEN
         pos = pos + 1
         res = WOD_UNDEF_REAL
         RETURN
      END IF

      READ(ascii(pos:pos),*) num_sig
      READ(ascii(pos+1:pos+1),*) num_total
      READ(ascii(pos+2:pos+2),*) num_prec
      pos = pos + 3

      !PRINT *, num_sig, num_total, num_prec
      READ(ascii(pos:pos+num_total-1),*) val_i
      res = val_i / 10.0**num_prec
      pos = pos + num_total
    END  FUNCTION readReal
    !===========================================================================


  END SUBROUTINE read_wod_rec
  !=============================================================================


END MODULE obs_reader_wod_mod
