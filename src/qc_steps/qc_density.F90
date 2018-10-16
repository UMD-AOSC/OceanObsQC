!===============================================================================
!>
!-------------------------------------------------------------------------------
MODULE qc_density_mod
  USE qc_step_mod
  USE profile_mod
  USE vec_profile_mod
  USE gsw_mod_toolbox

  IMPLICIT NONE
  PRIVATE

  !=============================================================================
  !-----------------------------------------------------------------------------
  TYPE, EXTENDS(qc_step), PUBLIC :: qc_density
   CONTAINS
     PROCEDURE, NOPASS :: name  => qc_step_name
     PROCEDURE, NOPASS :: desc  => qc_step_desc
     PROCEDURE, NOPASS :: init  => qc_step_init
     PROCEDURE, NOPASS :: check => qc_step_check
  END TYPE qc_density
  !=============================================================================

  ! parameters read in from namelist
  REAL :: dens_inv_tol = 0.0


CONTAINS



  !=============================================================================
  !> A short (~8 char) unique name for this QC plugin.
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_name() RESULT(name)
    CHARACTER(:), ALLOCATABLE :: name
    name = "qc_density"
  END FUNCTION qc_step_name
  !=============================================================================



  !=============================================================================
  !> A short, human friendly, description of what this QC step does.
  !! Should ideally fit on one line
  !-----------------------------------------------------------------------------
  FUNCTION qc_step_desc() RESULT(desc)
    CHARACTER(:), ALLOCATABLE :: desc
    desc = "check for density inversions"
  END FUNCTION qc_step_desc
  !=============================================================================



  !=============================================================================
  !> Perform initialization for this plugin.
  !! This subroutine is only called once, even if the qc_step_check
  !! subroutine is called multiple times.
  !! @param nmlfile  the unit number of the already open namelist file
  !-----------------------------------------------------------------------------
  SUBROUTINE qc_step_init(nmlfile)
    INTEGER, INTENT(in) :: nmlfile

    NAMELIST /qc_density/ dens_inv_tol
    READ(nmlfile, qc_density)
    PRINT qc_density

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
  SUBROUTINE qc_step_check(obs_in, obs_out, obs_rej)
    TYPE(vec_profile), INTENT(in)    :: obs_in
    TYPE(vec_profile), INTENT(inout) :: obs_out
    TYPE(vec_profile), INTENT(inout) :: obs_rej

    INTEGER :: i, j, nlev, k, l
    TYPE(profile), POINTER :: prof
    LOGICAL :: keep
    REAL(8), ALLOCATABLE :: pot_rho(:), sa(:), pres(:)

    INTEGER :: bad_densinv

    bad_densinv = 0
    do_prof: DO i = 1, obs_in%SIZE()
       prof => obs_in%of(i)
       keep = .TRUE.

       nlev = SIZE(prof%depth)

       ! if salinity and temperature profiles exist
       IF (SIZE(prof%temp) == SIZE(prof%salt)) THEN
          ALLOCATE(pres(nlev))
          ALLOCATE(pot_rho(nlev))
          ALLOCATE(sa(nlev))

          ! convert depth to pressure
          pres = gsw_p_from_z(-REAL(prof%depth, 8), prof%lat)

          ! convert practical salinity to absolute salinity
          sa=0
          WHERE(prof%salt < PROF_UNDEF) sa = prof%salt
          sa = gsw_sa_from_sp(sa, pres, prof%lon, prof%lat)

          ! calculate potential density
          pot_rho = gsw_pot_rho_t_exact(sa, REAL(prof%temp,8), pres, 0d0)


          ! find inversions
          !---------------------------------------------------------------------

          ! find first level with valid density
          j=1
          DO WHILE(j <= nlev)
             IF(prof%temp(j) < PROF_UNDEF .AND. prof%salt(j) < PROF_UNDEF) EXIT
             j = j + 1
          END DO

          ! for each valid level after this
          do_lvl: DO k=j+1, nlev
             IF(prof%temp(k) >= PROF_UNDEF .OR. prof%salt(k) >= PROF_UNDEF) CYCLE

             IF (pot_rho(j) - pot_rho(k) > dens_inv_tol) THEN

                ! if we found a density inversion, done use this profile
                bad_densinv = bad_densinv + 1
                keep = .FALSE.
                EXIT do_lvl

                ! TODO check to see if individual levels could be removed to
                ! fix the density inversion
             END IF
             j=k
          END DO do_lvl

          DEALLOCATE(pres, pot_rho, sa)
       END IF

       ! only keep if density inversion not found
       IF(keep) THEN
         CALL obs_out%push_back(prof)
       ELSE
         prof%tag = -1 !TODO, give valid tag
         CALL obs_rej%push_back(prof)
       END IF


    END DO do_prof

    ! print out stats if any profiles were removed
    IF(bad_densinv > 0)&
         PRINT '(I8,A)', bad_densinv, ' profiles removed for density inversion'

  END SUBROUTINE qc_step_check
  !=============================================================================

END MODULE qc_density_mod
