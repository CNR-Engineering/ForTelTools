!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: export the last frame of a Serafin file (2D or 3D)
! @info:
!   * only the last frame is read and export to optimize execution time
!   * time can be initialized to zero
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_LAST
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH, YES_OR_NO
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

      ! Variables defined by the user (read from standard input in this order)
      CHARACTER*80 :: inname, outname
      LOGICAL :: reset_time

      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51

      TYPE(RESDATA) :: resin, resout
      LOGICAL       :: eof
      REAL*4        :: time
      INTEGER       :: IFRAME, NFRAME
      REAL*4,  DIMENSION(:,:), ALLOCATABLE :: values

      CALL SCRIPT_HEADER("slf_last", "1.0",
     &  "Recuperer le dernier enregistrement temporel")

      ! Get filenames and open connections
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_last')
      CALL SLF_OPEN(outunit, outname, 'w', resout)

      ! Read, print and copy header
      CALL SLF_READ_HEADER(resin)
      CALL COPY_HEADER(resin, resout)
      CALL PRINT_HEADER(resin)
      CALL SLF_WRITE_HEADER(resout)

      NFRAME = NB_FRAME(resin)

      reset_time = YES_OR_NO("Initialize time to zero?", .TRUE.)

      ! Skip values until IFRAME.EQ.NFRAME
      ALLOCATE(values(resout%nnode,resout%nbvar))
      IFRAME = 0
 80   CALL SLF_READ_TIME(resin, time, eof)
      IFRAME = IFRAME + 1
      IF(eof) THEN
        WRITE(*,*) "ERROR: end of file should not be encountered"
        WRITE(*,*) "Loop is false or NFRAME is not coherant"
        STOP
      ELSE IF(IFRAME.NE.NFRAME) THEN
        CALL SLF_SKIP_FRAME(resin)
        GOTO 80
      ENDIF

      ! Read and write the time and the entire last frame
      values = SLF_READ_FRAME(resin)
      IF(reset_time) time = 0.D0
      CALL SLF_WRITE_FRAME(resout, time, values)

      CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_LAST
