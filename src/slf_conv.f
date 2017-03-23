!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: convert little/big endian telemac file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_CONV
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH, YES_OR_NO
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

      ! Variables defined by the user (read from standard input in this order)
      CHARACTER*80 :: inname, outname
      LOGICAL      :: conv_in, conv_out

      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51

      TYPE(RESDATA) :: resin, resout
      LOGICAL       :: eof
      REAL*4        :: time
      INTEGER       :: IFRAME
      REAL*4,  DIMENSION(:,:), ALLOCATABLE :: values

      CALL SCRIPT_HEADER("slf_conv", "1.0",
     &  "Convertir un fichier resultat")

      ! Get filenames and endian type and open connections
      inname = READ_ANSWER_STR("Input Selafin file")
      conv_in = YES_OR_NO("Convert input in Little Endian?", .FALSE.)
      CALL SLF_OPEN(inunit, inname, 'r', resin, conv_in)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_conv')
      conv_out = YES_OR_NO("Convert output in Little Endian?", .FALSE.)
      CALL SLF_OPEN(outunit, outname, 'w', resout, conv_out)

      ! Read, print and copy headers
      CALL SLF_READ_HEADER(resin)
      CALL COPY_HEADER(resin, resout)
      CALL PRINT_HEADER(resin)
      CALL SLF_WRITE_HEADER(resout)

      ! Read/write or skip values for each record time
      ALLOCATE(values(resout%nnode,resout%nbvar))
      IFRAME = 0

 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof) GOTO 90
      values = SLF_READ_FRAME(resin)
      CALL SLF_WRITE_FRAME(resout, time, values)
      IFRAME = IFRAME + 1
      GOTO 80

 90   WRITE(*,*) IFRAME,'frames wrote in ',TRIM(outname)
      CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_CONV
