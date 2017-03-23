!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: module to read and write Serafin files from Telemac2D and 3D
!
! TODO:
!   * optimize SLF_GET_TIME_SERIE (with stream? and inquire?)
!   * handle errors in reading/writing
!
! Contains subroutines/functions:
!   * SUBROUTINE SLF_OPEN
!   * SUBROUTINE SLF_READ_HEADER
!   * FUNCTION   NB_FRAME
!   * SUBROUTINE SLF_GET_TIME_SERIE
!   * SUBROUTINE SLF_READ_TIME
!   * FUNCTION   SLF_READ_FRAME
!   * SUBROUTINE SLF_SKIP_FRAME
!   * FUNCTION   SLF_READ_VARS_FRAME
!   * SUBROUTINE SLF_WRITE_HEADER
!   * SUBROUTINE SLF_WRITE_FRAME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE SERAFIN

      USE COMMON_DATA, ONLY: RESDATA
      IMPLICIT NONE

      CONTAINS
!***********************************************************************
      SUBROUTINE SLF_OPEN(channel, filename, mode, res, convert)
      IMPLICIT NONE
      INTEGER,       INTENT(IN)  :: channel
      CHARACTER*80,  INTENT(IN)  :: filename
      CHARACTER*1,   INTENT(IN)  :: mode
      TYPE(RESDATA), INTENT(OUT) :: res
      LOGICAL, INTENT(IN), OPTIONAL :: convert

      CHARACTER*32 :: endian

      endian = 'BIG_ENDIAN' ! default value
      IF(PRESENT(convert)) THEN
        IF(convert) endian = 'LITTLE_ENDIAN'
      ENDIF

!      WRITE(*,*) "Open ",TRIM(filename)," with ",mode," mode in channel",channel

      IF(mode.EQ.'r') THEN
        OPEN(channel, file=TRIM(filename), status='old',
     &    form='unformatted', convert=TRIM(endian), access='sequential')
      ELSE IF(mode.EQ.'w') THEN
!       Will crash (Fotran runtime error) if file already exists (because status="new")
        OPEN(channel, file=TRIM(filename), status='new',
     &    form='unformatted', convert=TRIM(endian), access='sequential')
      ELSE
        WRITE(*,*) "ERROR: unknow mode to open slf file"
        STOP
      ENDIF

      res%filename = filename
      res%id = channel

      END SUBROUTINE SLF_OPEN
!***********************************************************************
      SUBROUTINE SLF_READ_HEADER(res)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(INOUT) :: res
      INTEGER :: I

      READ(res%id) res%title
      READ(res%id) res%nbvar, res%nbvar2

      IF(.NOT.ALLOCATED(res%varnames)) ALLOCATE(res%varnames(res%nbvar))
      DO I = 1, res%nbvar
        READ(res%id) res%varnames(I)
      ENDDO

      READ(res%id) res%param(:)

      IF(res%param(10).EQ.1) THEN
        READ(res%id) res%date(:)
      ENDIF

      READ(res%id) res%nelem, res%nnode, res%ndp, res%i

      IF(.NOT.ALLOCATED(res%ikle)) ALLOCATE(res%ikle(res%ndp,res%nelem))
      READ(res%id) (res%ikle(:,I), I=1, res%nelem)

      IF(.NOT.ALLOCATED(res%ipobo)) ALLOCATE(res%ipobo(res%nnode))
      READ(res%id) res%ipobo(:)

      IF(.NOT.ALLOCATED(res%x)) ALLOCATE(res%x(res%nnode))
      READ(res%id) res%x(:)

      IF(.NOT.ALLOCATED(res%y)) ALLOCATE(res%y(res%nnode))
      READ(res%id) res%y(:)

      res%nplan = res%param(7)
      IF(res%nplan.GT.0) THEN
        res%meshtype = '3D'
        res%nnode2d = res%nnode/res%nplan
      ELSE
        res%meshtype = '2D'
        res%nnode2d = res%nnode
      ENDIF

      END SUBROUTINE SLF_READ_HEADER
!***********************************************************************
      INTEGER FUNCTION NB_FRAME(res)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN)  :: res
      INTEGER :: headersize, filesize, framesize

      filesize = 0              !FIXME: debug for windows

      ! filesize
      !INQUIRE(FILE=TRIM(res%filename), SIZE=filesize)
      INQUIRE(UNIT=res%id, SIZE=filesize)
      IF(filesize.LT.0) THEN
        WRITE(*,*) "ERROR: file size could not be read"
        WRITE(*,*) "Value is", filesize
        STOP
      ENDIF

      ! headersize
      headersize = (80+8) + (2*4+8) + res%nbvar*(32+8) + (4*10+8) +
     &  (4*4+8) + (res%nelem*res%ndp*4+8) + 3*(res%nnode*4+8)
      IF(res%param(10).EQ.1) THEN ! date is present
        headersize = headersize + 6*4+8
      ENDIF

      ! framesize
      framesize = (4+8) + res%nbvar*(res%nnode*4+8)

      ! Deduce and check NB_FRAME
      NB_FRAME = (filesize-headersize)/framesize
      IF(filesize-headersize.NE.framesize*NB_FRAME) THEN
        WRITE(*,*) "File is corrupted or not complete"
        STOP
      ENDIF

      END FUNCTION NB_FRAME
!***********************************************************************
      SUBROUTINE SLF_GET_TIME_SERIE(unit, filename, time_serie)
      USE COMMON_DATA, ONLY: CLOSE_RES
      IMPLICIT NONE
      INTEGER,      INTENT(IN) :: unit
      CHARACTER*80, INTENT(IN) :: filename
      DOUBLE PRECISION, ALLOCATABLE, INTENT(OUT) :: time_serie(:)

      TYPE(RESDATA) :: res
      INTEGER :: I, NFRAME
      REAL*4  :: time
      LOGICAL :: eof

      CALL SLF_OPEN(unit, filename, 'r', res)
      CALL SLF_READ_HEADER(res)

      NFRAME = NB_FRAME(res)
      ALLOCATE(time_serie(NFRAME))
      I = 0
 80   CALL SLF_READ_TIME(res, time, eof)
      IF(eof) GOTO 90
      CALL SLF_SKIP_FRAME(res)
      I = I + 1
      time_serie(I) = time
      GOTO 80
 90   CALL CLOSE_RES(res, deallocate_res=.TRUE.)

      END SUBROUTINE SLF_GET_TIME_SERIE
!***********************************************************************
      SUBROUTINE SLF_READ_TIME(res, time, eof)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN)  :: res
      REAL*4, INTENT(OUT) :: time
      LOGICAL, INTENT(OUT) :: eof

      READ(res%id, end=999) time
      WRITE(*,*) "Time",time
      eof = .FALSE.
      RETURN
 999  WRITE(*,*) 'End of file encountered'
      eof = .TRUE.
      END SUBROUTINE SLF_READ_TIME
!***********************************************************************
      FUNCTION SLF_READ_FRAME(res) RESULT(values)
!     @brief: read an entire frame (ie all variables)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN)  :: res
      REAL*4, DIMENSION(res%nnode,res%nbvar) :: values

      INTEGER :: I

      WRITE(*,*) 'Read',res%nbvar,'variable(s)'
      DO I = 1, res%nbvar
        READ(res%id, end=900) values(:,I)
      ENDDO

      RETURN
 900  WRITE(*,*) "File is probably corrupted (a frame is not complete)"
      STOP
      END FUNCTION SLF_READ_FRAME
!***********************************************************************
      SUBROUTINE SLF_SKIP_FRAME(res)
!     @brief: TODO
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      INTEGER :: I

      WRITE(*,*) 'Skip entire frame'
      DO I = 1, res%nbvar
        READ(res%id, end=900)
      ENDDO

      RETURN
 900  WRITE(*,*) "File is probably corrupted (a frame is not complete)"
      STOP
      END SUBROUTINE SLF_SKIP_FRAME
!***********************************************************************
      FUNCTION SLF_READ_VARS_FRAME(res, var2read) RESULT(values)
!     @brief: read only var2read in res
!     (/!\ size of values in coherance with var2read)
!     Is significantly faster than SLF_READ_FRAME if there is
!     only few variables to keep
!     /!\ ORDER IN VAR2READ IS ESSENTIAL
!     FIXME: SAVE a table with J (avoid calling MINLOC each time the function is called)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      INTEGER, DIMENSION(:) :: var2read
      REAL*4, DIMENSION(res%nnode,res%nbvar) :: values

      INTEGER :: I, J

      DO I = 1, res%nbvar
        IF(ANY(var2read.EQ.I)) THEN
          J = MINLOC(ABS(var2read - I), 1) ! current variable position in var2read
          READ(res%id, end=900) values(:,J)
        ELSE
          READ(res%id, end=900) ! without assigning
        ENDIF
      ENDDO
      WRITE(*,*) 'Read ',SIZE(var2read),'variables among',res%nbvar

      RETURN
 900  WRITE(*,*) "File is probably corrupted (a frame is not complete)"
      STOP
      END FUNCTION SLF_READ_VARS_FRAME
!***********************************************************************
      SUBROUTINE SLF_WRITE_HEADER(res)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      INTEGER :: I

      WRITE(res%id) res%title
      WRITE(res%id) res%nbvar, res%nbvar2

      DO I = 1, res%nbvar
        WRITE(res%id) res%varnames(I)
      ENDDO

      WRITE(res%id) res%param
      IF(res%param(10).EQ.1) THEN
        WRITE(res%id) res%date
      ENDIF
      WRITE(res%id) res%nelem, res%nnode, res%ndp, res%i

      WRITE(res%id) (res%ikle(:,I), I=1, res%nelem)
      WRITE(res%id) res%ipobo

      WRITE(res%id) res%x
      WRITE(res%id) res%y

      END SUBROUTINE SLF_WRITE_HEADER
!***********************************************************************
      SUBROUTINE SLF_WRITE_FRAME(res, time, values)
!     @brief: TODO
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      REAL*4, INTENT(IN) :: time
      REAL*4, DIMENSION(res%nnode,res%nbvar), INTENT(IN) :: values
      INTEGER :: I

      WRITE(*,*) 'Write time',time
      WRITE(res%id) time
      DO I = 1, res%nbvar
        WRITE(res%id) values(:,I)
      ENDDO

      END SUBROUTINE SLF_WRITE_FRAME

      END MODULE SERAFIN
