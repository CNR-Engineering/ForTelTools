!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: apply simple transformation(s) to mesh
!   Possible transformations are: translation, rotation, homothety
! @info: copy all the variables and frames without any modification
! @warnings:
!   * order of transformations are relevant :
!       translation, rotation and finally homothety
!   * node order may not be Serafin standard? (in case of rotation)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_TRANSFMESH
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH,
     &  YES_OR_NO, STDIN_REAL, STDIN_DBLE
      USE COMMON_DATA
      USE SERAFIN
      USE GEOM2D, ONLY: ROTATION, HOMOTHETY
      IMPLICIT NONE

!     Variables defined by the user
      CHARACTER*80 :: inname, outname
      REAL*4 :: shift_X, shift_Y, Xc, Yc, angle, ratio

!     Local variables
      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51
      TYPE(RESDATA) :: resin, resout
      LOGICAL :: eof
      INTEGER :: IFRAME
      REAL*4  :: time
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: values
      REAL*4, DIMENSION(:), ALLOCATABLE :: X, Y !FIXME: usefull?

      CALL SCRIPT_HEADER("slf_transfmesh", "1.0",
     &  "Transformer le maillage horizontal")

      ! Get filenames and open connections
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_tranf')
      CALL SLF_OPEN(outunit, outname, 'w', resout)

      ! Read, print and copy header
      CALL SLF_READ_HEADER(resin)
      CALL COPY_HEADER(resin, resout)
      CALL PRINT_HEADER(resin)

      ALLOCATE(X(resout%nnode))
      ALLOCATE(Y(resout%nnode))

      IF(YES_OR_NO("Translate mesh ?", .FALSE.)) THEN
        shift_X = STDIN_REAL("shift X [0]", 0.)
        shift_Y = STDIN_REAL("shift Y [0]", 0.)

        resout%x = resout%x + shift_X
        resout%y = resout%y + shift_Y
      ENDIF

      IF(YES_OR_NO("Rotate mesh ?", .FALSE.)) THEN
        ! Make a copy
        X = resout%x
        Y = resout%y

        Xc = STDIN_REAL("X coordinate of center", 0.)
        Yc = STDIN_REAL("Y coordinate of center", 0.)
        angle = STDIN_REAL("Angle in degrees (anticlockwise sense)", 0.)
        CALL ROTATION(resout%nnode, X, Y, resout%x, resout%y, Xc, Yc, angle)
      ENDIF

      IF(YES_OR_NO("Homothety (or homothecy, or homogeneous dilation) ?",
     &  .FALSE.)) THEN
        ! Make a copy
        X = resout%x
        Y = resout%y

        Xc = STDIN_REAL("X coordinate of center", 0.) ! FIXME: use same center as before if rotate
        Yc = STDIN_REAL("Y coordinate of center", 0.)
        ratio = STDIN_REAL("Ratio (1=identity)", 1.)
        CALL HOMOTHETY(resout%nnode, X, Y, resout%x, resout%y, Xc, Yc, ratio)
      ENDIF

      DEALLOCATE(X)
      DEALLOCATE(Y)

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

      END PROGRAM SLF_TRANSFMESH
