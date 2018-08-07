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
    type(profile), intent(out) :: ob
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

    integer :: var_code(10)
    valid = .TRUE.


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

    ! get cast number
    tmp_i = readInt(); !   PRINT '(A10, I10)', "Cast: ", tmp_i

    ! country code (ignore)
    !PRINT *, "Country: ", ascii(pos:pos+1)
    pos = pos + 2

    tmp_i = readInt();  ! PRINT '(A10, I10)', "Cruise: ", tmp_i
    tmp_i = readInt(4); ! PRINT '(A10, I10)', "Year: ", tmp_i
    tmp_i = readInt(2); ! PRINT '(A10, I10)', "Month: ", tmp_i
    tmp_i = readInt(2); ! PRINT '(A10, I10)', "Day: ", tmp_i
    tmp_r = readReal(); ! PRINT '(A10, F10.4)', "Time: ", tmp_r
    tmp_r = readReal(); ! PRINT '(A10, F10.4)', "Lat: ", tmp_r
    tmp_r = readReal(); ! PRINT '(A10, F10.4)', "Lon: ", tmp_r
    num_lvl = readInt();!   PRINT '(A10, I10)', "levels: ", num_lvl
    ALLOCATE(ob%depth(num_lvl))
    tmp_i = readInt(1); ! PRINT '(A10, I10)', "prof_type: ", tmp_i ! O if 0, S if 1

    
    num_var = readInt(2) ! number of variables
    DO i = 1, num_var
       var_code(i) = readInt() ! variable type code
       if(var_code(i) == 1) then ! temp
          allocate(ob%temp(num_lvl))
       else if(var_code(i) == 2) then
          allocate(ob%salt(num_lvl))
       end if
       
       tmp_i = readInt(1) ! variable qc
       num_var_meta = readInt() ! number of var meta data entries
       DO j=1, num_var_meta
          tmp_i = readInt() ! var meta type code
          tmp_r = readReal() ! var meta value
       END DO
    END DO

    if (.not. allocated(ob%temp)) allocate(ob%temp(0))
    if (.not. allocated(ob%salt)) allocate(ob%salt(0))

    
    ! --------------------------------------------------------------------------
    ! Read character data and PI header
    ! --------------------------------------------------------------------------
    n = readInt() ! character data length
    IF (n /= WOD_UNDEF_INT) THEN
       pos = pos + n
    END IF


    ! --------------------------------------------------------------------------
    ! Secondary header
    ! --------------------------------------------------------------------------
    n = readInt() ! second header length
    IF (n /= WOD_UNDEF_INT) THEN
       pos = pos + n
    END IF


    ! --------------------------------------------------------------------------
    ! Biological header
    ! --------------------------------------------------------------------------
    n = readInt() ! bio header length
    IF (n /= WOD_UNDEF_INT) THEN
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
          if(var_code(j) == 1) then
             ob%temp(i) = val
          else if(var_code(j) == 2) then
             ob%salt(i) = val
          end if
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
