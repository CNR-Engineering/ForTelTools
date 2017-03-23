!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: subset a Serafin (2D or 3D) file by choosing
!   variables and time serie to write
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_ANAL
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH,
     &  YES_OR_NO, STDIN_INT, STDIN_DBLE, STDIN_ARRAY_INT
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

      ! Variables defined by the user (read from standard input in this order)
      CHARACTER*80     :: inname, outname
      DOUBLE PRECISION :: min_time, max_time
      INTEGER          :: ech

      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51

      TYPE(RESDATA) :: resin, resout
      LOGICAL       :: eof, sup
      REAL*4        :: time

      INTEGER       :: IFRAME
      REAL*4,  DIMENSION(:,:), ALLOCATABLE :: values
      INTEGER, DIMENSION(:),   ALLOCATABLE :: var2del, var2keep

      CALL SCRIPT_HEADER("slf_anal", "1.0",
     &  "Extraire une partie d'un fichier resultat")

      ! Read file names and open connections
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_anal')
      CALL SLF_OPEN(outunit, outname, 'w', resout)

      CALL SLF_READ_HEADER(resin)
      CALL COPY_HEADER(resin, resout)

      sup = YES_OR_NO('Delete some variable(s)?', .FALSE.)
      CALL PRINT_HEADER(resin)

      IF(sup) THEN
        ! Some variable(s) have to be removed
        WRITE(*,*) "Enter variable index(es) to delete (0 to finish)"
        CALL STDIN_ARRAY_INT(resin%nbvar, 0, var2del)
        CALL VARS_TO_KEEP(resin, var2del, var2keep)
        DEALLOCATE(var2del)
        CALL ASSIGN_VARS(resin, resout, var2keep)
      ENDIF

      CALL SLF_WRITE_HEADER(resout)

      ! Read user configuration
      min_time = STDIN_DBLE("First time to be treated", -1.D38, "start")
      max_time = STDIN_DBLE("Last time to be treated", 1.D38, "end")
      ech = STDIN_INT("Sampling value [1]", 1)

      ! Read/write or skip values for each record time
      ALLOCATE(values(resout%nnode,resout%nbvar))
      IFRAME = 0

 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof.OR.time.GT.max_time) GOTO 90
      IF(time.GE.min_time.AND.MODULO(IFRAME, ech).EQ.0) THEN
        IF(sup) THEN
          values = SLF_READ_VARS_FRAME(resin, var2keep)
        ELSE
          values = SLF_READ_FRAME(resin)
        ENDIF
        CALL SLF_WRITE_FRAME(resout, time, values)
      ELSE
        CALL SLF_SKIP_FRAME(resin)
      ENDIF
      IFRAME = IFRAME + 1
      GOTO 80

 90   WRITE(*,*) IFRAME,'frames wrote in ',TRIM(outname)
      CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_ANAL
