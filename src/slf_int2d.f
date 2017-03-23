!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: interpolate Serafin-2D on a set of probes/points
! @info:
!   * only the selected variables are exported
!   * ponderations (interpolation function) are not time dependant
!       and are computed once at the beginning
!
! TODO:
!   * default output filename (outname)
!   * export as true CSV format with header
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE READ_PROBE_DATA(probunit, probname, interp)
!     @brief: read ascii file with probe characteristics and assign them
      USE INT_TABLES, ONLY: INTERP_DATA
      IMPLICIT NONE
      INTEGER,      INTENT(IN) :: probunit
      CHARACTER*80, INTENT(IN) :: probname
      TYPE(INTERP_DATA), INTENT(OUT) :: interp

      INTEGER :: I

      OPEN(unit=probunit, file=TRIM(probname))
      READ(probunit,*) interp%nb_probes
      ALLOCATE(interp%Xprobe(interp%nb_probes))
      ALLOCATE(interp%Yprobe(interp%nb_probes))
      DO I = 1, interp%nb_probes
        READ(probunit,*) interp%Xprobe(I), interp%Yprobe(I)
      ENDDO
      CLOSE(probunit)

      END SUBROUTINE READ_PROBE_DATA
!***********************************************************************
      PROGRAM SLF_INT2D
      USE READ_STDIN, ONLY: READ_ANSWER_STR, STDIN_ARRAY_INT
      USE INT_TABLES
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

      CHARACTER*80 :: inname, probname, outpattern, outname

      INTEGER, PARAMETER :: inunit = 50
      INTEGER, PARAMETER :: probunit = 51
      INTEGER, PARAMETER :: outunit_start = 52

      INTEGER, DIMENSION(:), ALLOCATABLE :: outunit
      REAL, DIMENSION(:,:), ALLOCATABLE :: values
      REAL, DIMENSION(:), ALLOCATABLE :: val_int

      TYPE(RESDATA) :: resin
      LOGICAL       :: eof
      REAL          :: time
      INTEGER       :: IFRAME, I
      CHARACTER*3   :: var_str ! format '(I3.3)' is hardcoded

      INTEGER :: nb_var_int
      INTEGER, DIMENSION(:), ALLOCATABLE :: var2write
      TYPE(INTERP_DATA) :: interp

      CALL SCRIPT_HEADER("slf_int2d", "1.0",
     &  "Interpoler des variables sur un semis de points")

      ! Open files from user inputs
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)

      probname = READ_ANSWER_STR("Input probe file")
      outpattern = READ_ANSWER_STR("Output pattern")

      CALL SLF_READ_HEADER(resin)

      CALL PRINT_HEADER(resin)
      WRITE(*,*) "Enter variable index(es) to interpolate (0 to finish)"
      CALL STDIN_ARRAY_INT(resin%nbvar, 0, var2write)
      nb_var_int = SIZE(var2write)

      IF(nb_var_int.EQ.0) THEN
        WRITE(*,*) "All variables are exported"
        nb_var_int = resin%nbvar
        DEALLOCATE(var2write)
        ALLOCATE(var2write(nb_var_int))
        var2write = (/ (I, I=1, nb_var_int) /)
      ENDIF

      ! Open output files
      ALLOCATE(outunit(nb_var_int))
      DO I = 1, nb_var_int
        WRITE(var_str,'(I3.3)') var2write(I)
        outunit(I) = outunit_start+I-1
        outname = TRIM(outpattern)//'_'//TRIM(var_str)//'.csv'
        OPEN(outunit(I), file=TRIM(outname))
      ENDDO

      CALL READ_PROBE_DATA(probunit, probname, interp)
      ! Build interpolation tables and allocate result arrays
      CALL BUILD_INT_TABLES(resin, interp)

      ALLOCATE(values(resin%nnode,nb_var_int))
      ALLOCATE(val_int(interp%nb_probes))

      ! Read values and write interpolated values for each record time
      IFRAME = 0
 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof) GOTO 90
      values = SLF_READ_VARS_FRAME(resin, var2write)
      DO I = 1, nb_var_int
        val_int = INTERP_FROM_INT_TABLES(values(:,I), interp)
        WRITE(outunit(I),'(1000(E16.8,'';'',:))') time, val_int !FIXME: format is hardcoded
      ENDDO
      IFRAME = IFRAME + 1
      GOTO 80

 90   CALL CLOSE_RES(resin)
      DO I = 1, nb_var_int
         CLOSE(outunit(I))
      ENDDO

      END PROGRAM SLF_INT2D
