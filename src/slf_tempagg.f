!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: temporal aggregation
!   Possible aggregations are: 1=min, 2=mean, 3=max
! @info: mean is computed through a sum => less precise with large number of frames
! @warning: mean is meaningful only if the time step is constant !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_TEMPAGG
      USE READ_STDIN, ONLY: READ_ANSWER_STR, YES_OR_NO, STDIN_INT,
     &  STDIN_ARRAY_INT
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

!     Variables defined by the user
      CHARACTER*80 :: inname, outname
      INTEGER, DIMENSION(:),   ALLOCATABLE :: var2anal
      INTEGER :: aggtype
      LOGICAL :: reset_time

!     Local variables
      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51
      CHARACTER*80 :: outname_default
      TYPE(RESDATA) :: resin, resout
      LOGICAL :: eof
      INTEGER :: IFRAME
      REAL*4 :: time
      REAL*4,  DIMENSION(:,:), ALLOCATABLE :: values, cur_agg

      CALL SCRIPT_HEADER("slf_tempagg", "1.0",
     &  "Agreger temporellement en chaque noeud (min/moy/max)")

!     Read file names
      inname = READ_ANSWER_STR("Input Selafin file")

!     Open connection and start reading resin
      CALL SLF_OPEN(inunit, inname, 'r', resin)
      CALL SLF_READ_HEADER(resin)
      CALL PRINT_HEADER(resin)

!     Analyze only some variables
      WRITE(*,*) "Enter variable(s) to analyze (0 to finish)"
      CALL STDIN_ARRAY_INT(resin%nbvar, 0, var2anal)

      WRITE(*,*) "Temporal aggregation type?"
      WRITE(*,*) "1) Minimum"
      WRITE(*,*) "2) Mean"
      WRITE(*,*) "3) Maximum"
      aggtype = STDIN_INT("Desired operation:")

!     Build outname_default
      outname_default = inname
      SELECT CASE (aggtype)
        CASE (1)
          CALL STR_ADD_SUFFIX(outname_default, '_min')
        CASE (2)
          CALL STR_ADD_SUFFIX(outname_default, '_mean')
        CASE (3)
          CALL STR_ADD_SUFFIX(outname_default, '_max')
        CASE DEFAULT
          WRITE(*,*) "ERROR: aggregation type is unknown"
          STOP
      END SELECT

      outname = READ_ANSWER_STR("Output Selafin file", outname_default)

      CALL SLF_OPEN(outunit, outname, 'w', resout)
      CALL COPY_HEADER(resin, resout)
      CALL ASSIGN_VARS(resin, resout, var2anal)
      CALL SLF_WRITE_HEADER(resout)

      reset_time = YES_OR_NO("Initialize time to zero?", .TRUE.)

!     Read/write or skip values for each record time
      ALLOCATE(values(resout%nnode, SIZE(var2anal)))
      ALLOCATE(cur_agg(resout%nnode, SIZE(var2anal)))
      IFRAME = 0

 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof) GOTO 90
      values = SLF_READ_VARS_FRAME(resin, var2anal)
      IFRAME = IFRAME + 1

      IF(IFRAME.EQ.0) THEN
!       First iteration
        cur_agg(:,:) = values(:,:)
      ELSE
!       Other iteration
        SELECT CASE (aggtype)
        CASE (1)
          cur_agg(:,:) = MIN(cur_agg, values)
        CASE (2)
          cur_agg(:,:) = cur_agg(:,:) + values(:,:)
        CASE (3)
          cur_agg(:,:) = MAX(cur_agg, values)
        CASE DEFAULT
!         Already checked
        END SELECT
      ENDIF

      GOTO 80

 90   IF(aggtype.EQ.2) THEN
!       Divide the sum by the number of frame
        cur_agg(:,:) = cur_agg(:,:)/IFRAME
      ENDIF

      IF(reset_time) time = 0.D0
      CALL SLF_WRITE_FRAME(resout, time, cur_agg)

      CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_TEMPAGG
