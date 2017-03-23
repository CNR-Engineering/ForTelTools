!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: compute simple transformation to the values
! @info: ...
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_TRANSFVALUES
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH,
     &  YES_OR_NO, STDIN_INT, STDIN_REAL, STDIN_ARRAY_INT
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

!     Variables defined by the user
      CHARACTER*80 :: inname, outname
      INTEGER, DIMENSION(:), ALLOCATABLE :: var2transf
      REAL*4 :: a, b

!     Local variables
      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51
      TYPE(RESDATA) :: resin, resout
      LOGICAL :: eof
      INTEGER :: IFRAME, I, varID
      REAL*4  :: time
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: values

      CALL SCRIPT_HEADER("slf_transfvalues", "1.0",
     &  "Remplacer des variables par relation affine")

!     Read files
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)
      CALL SLF_READ_HEADER(resin)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_transfv')
      CALL SLF_OPEN(outunit, outname, 'w', resout)
      CALL COPY_HEADER(resin, resout)

      CALL PRINT_HEADER(resin)
      WRITE(*,*) "Enter variable index(es) to transform (0 to finish)"
      CALL STDIN_ARRAY_INT(resin%nbvar, 0, var2transf)

!     Define transformation
      WRITE(*,*) "Affine transformation : x <- a*x + b"
      a = STDIN_REAL("a =", 1.0)
      b = STDIN_REAL("b =", 0.0)

      CALL SLF_WRITE_HEADER(resout)

!     Read/write values for each record time
      ALLOCATE(values(resout%nnode, resin%nbvar))
      IFRAME = 0

 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof) GOTO 90
      values = SLF_READ_FRAME(resin)

!     Compute transformed values
      DO I = 1, SIZE(var2transf)
        varID = var2transf(I)
        values(:,varID) = a*values(:,varID) + b
      ENDDO

!     Write transformed values
      CALL SLF_WRITE_FRAME(resout, time, values)
      IFRAME = IFRAME + 1
      GOTO 80

 90   CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_TRANSFVALUES
