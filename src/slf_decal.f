!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: shift X and/or Y mesh coordinates of a Serafin (2D or 3D) file
! @info: copy all the variables and frames without any modification
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_DECAL
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH,
     &  YES_OR_NO, STDIN_REAL, STDIN_DBLE
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

      ! Variables defined by the user
      CHARACTER*80 :: inname, outname
      REAL*4 :: shift_X, shift_Y

!     Local variables
      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51
      TYPE(RESDATA) :: resin, resout
      LOGICAL :: eof
      INTEGER :: IFRAME
      REAL*4 :: time
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: values

      CALL SCRIPT_HEADER("slf_decal", "1.0",
     &  "Translater le maillage horizontal")

      ! Read filenames and open file connections
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_decal')
      CALL SLF_OPEN(outunit, outname, 'w', resout)

      shift_X = STDIN_REAL("shift X", 0.)
      shift_Y = STDIN_REAL("shift Y", 0.)

      ! Read, print and copy header
      CALL SLF_READ_HEADER(resin)
      CALL COPY_HEADER(resin, resout)
      resout%x = resout%x + shift_X
      resout%y = resout%y + shift_Y
      CALL PRINT_HEADER(resin)
      CALL SLF_WRITE_HEADER(resout)

      ALLOCATE(values(resout%nnode,resout%nbvar))
      IFRAME = 0
 80   CALL SLF_READ_TIME(resin, time, eof)
      IFRAME = IFRAME + 1
      IF(eof) GOTO 90
      values = SLF_READ_FRAME(resin)
      CALL SLF_WRITE_FRAME(resout, time, values)
      GOTO 80

 90   CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_DECAL
