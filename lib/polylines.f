!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: handle polylign files
!
! CONTAINS
!   * SUBROUTINE WRITE_xyz
!   * SUBROUTINE READ_i2s
!   * SUBROUTINE WRITE_i2s
!   * ALLOC_PT2D_SET
!   * READ_i2s_PT2D_SET
!   * i2s_NB_POLY
!   * PRINT_PT2D_SET
!
! TODO: handle SX files (no convention?)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE POLYLINES

      TYPE PT2D_SET
        INTEGER :: nsom
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: x
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: y
      END TYPE

      PRIVATE :: i2s_NB_POLY

      CONTAINS
!***********************************************************************
      SUBROUTINE WRITE_XYZ(unit, nsom, X, Y, Z)
      IMPLICIT NONE
!     @brief: read xyz file with space/comma separator (read with free fortran format)
!     @prerequisites: SIZE(X)=SIZE(Y)=SIZE(Z)
      INTEGER, INTENT(IN) :: unit
      INTEGER, INTENT(IN) :: nsom
      DOUBLE PRECISION, DIMENSION(nsom), INTENT(IN) :: X, Y
      DOUBLE PRECISION, DIMENSION(nsom), INTENT(IN), OPTIONAL :: Z

      INTEGER :: isom

      DO isom = 1, nsom
        IF(PRESENT(Z)) THEN
          WRITE(unit,*) X(isom), Y(isom), Z(isom)
        ELSE
          WRITE(unit,*) X(isom), Y(isom)
        ENDIF
      ENDDO

      END SUBROUTINE WRITE_XYZ
!***********************************************************************
      SUBROUTINE READ_i2s(unit, MAX_PTS, NSOM, NPOLY, X, Y)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: read coordinates from a i2s (BlueKenue) polyline connection
!
! @prerequisites:
!  * column separator = space(s?)
!  * commented lines start with '#' or ':' (see COMMENT_CHAR array)
!  * once an uncommented line is found, no other commented line is possible
!  * only 2 columns are read : x and y coordinates
!  * line width <= 999 (or change LINE_FMT and LINE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE

!     IN/OUT
      INTEGER,          INTENT(IN)  :: unit
      INTEGER,          INTENT(IN)  :: MAX_PTS
      INTEGER,          INTENT(OUT) :: NSOM(NPOLY)
      INTEGER,          INTENT(IN)  :: NPOLY
      DOUBLE PRECISION, INTENT(OUT) :: X(NPOLY, MAX_PTS)
      DOUBLE PRECISION, INTENT(OUT) :: Y(NPOLY, MAX_PTS)

!     LOCAL VARIABLES
      CHARACTER(1), DIMENSION(2) :: COMMENT_CHAR = (/ '#', ':' /)
      CHARACTER(LEN=999) :: LINE
      CHARACTER(6) :: LINE_FMT = '(A999)'
      INTEGER :: IPOLY, ISOM, tempo, I

      DO IPOLY = 1, NPOLY
 100    READ(unit,LINE_FMT) LINE

        ! Ignore lines starting by a comment character
        DO I = 1, SIZE(COMMENT_CHAR)
          IF(LINE(1:1).EQ.COMMENT_CHAR(I)) THEN
            GOTO 100
          ENDIF
        ENDDO

        ! Read polyline caracteristics
        READ(LINE,*) NSOM(IPOLY), tempo
        WRITE(*,*) 'Read polyline',IPOLY,'with',NSOM(IPOLY),'values'

        ! Check polyline size
        IF(NSOM(IPOLY).GT.MAX_PTS) THEN
          WRITE(*,*) 'MAX_PTS (=', MAX_PTS, ') is exceed'
          WRITE(*,*) 'Polyline',IPOLY,'has',NSOM(IPOLY),'points'
          STOP
        ENDIF

        ! Read x, y values
        DO ISOM = 1, NSOM(IPOLY)
          READ(unit,LINE_FMT) LINE
          READ(LINE,*) X(IPOLY,ISOM), Y(IPOLY,ISOM)
        ENDDO

      ENDDO

      END SUBROUTINE READ_i2s
!***********************************************************************
      SUBROUTINE WRITE_i2s(unit, name, NSOM, NPOLY, X, Y)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: write i2s (BlueKenue) from polyline coordinates
! @prerequisites: SIZE(X)=SIZE(Y) and in coherance with NPOLY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE

!     IN/OUT
      INTEGER, INTENT(IN) :: unit
      CHARACTER*80, INTENT(IN) :: name
      INTEGER, INTENT(IN) :: NSOM(NPOLY)
      INTEGER, INTENT(IN) :: NPOLY
      DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN) :: X, Y

      REAL, PARAMETER :: value = 0.
      INTEGER :: IPOLY, ISOM

!     Write header
      WRITE(unit,*) "# Test"
      WRITE(unit,*) "#########################################################################"
      WRITE(unit,*) ":FileType i2s  ASCII  EnSim 1.0"
      WRITE(unit,*) "# DataType                 2D Line Set"
      WRITE(unit,*) "#"
      WRITE(unit,*) "#------------------------------------------------------------------------"
      WRITE(unit,*) ":Name "//TRIM(name)
      WRITE(unit,*) "#"
      WRITE(unit,*) ":AttributeUnits 1 m"
      WRITE(unit,*) ":EndHeader"

      DO IPOLY = 1, NPOLY
        WRITE(unit,*) NSOM(IPOLY), value !FIXME: without leading spaces

        DO ISOM = 1, NSOM(IPOLY)
!         Write coordinates for each node of each polylines
          WRITE(unit,*) X(IPOLY,ISOM), Y(IPOLY,ISOM)
        ENDDO
      ENDDO

      END SUBROUTINE WRITE_i2s
!
!=======================================================================
!     MANIPULATION PT2D_SET VARIABLES
!=======================================================================
!
      SUBROUTINE ALLOC_PT2D_SET(nsom, pt_set)
!     @brief: allocate (without initialization) pt_set arrays with nsom
      IMPLICIT NONE
      TYPE(PT2D_SET), INTENT(OUT) :: pt_set
      INTEGER, INTENT(IN) :: nsom

      pt_set%nsom = nsom
      ALLOCATE(pt_set%x(nsom))
      ALLOCATE(pt_set%y(nsom))

      END SUBROUTINE ALLOC_PT2D_SET
!***********************************************************************
      SUBROUTINE READ_i2s_PT2D_SET(filename, pt_set_array, npoly)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: read coordinates from a i2s (BlueKenue) polyline file
!
! @prerequisites:
!  * /!\/!\/!\ pt_set_array should not be already allocated /!\/!\/!\
!  * column separator = space(s?)
!  * commented lines start with '#' or ':' (see COMMENT_CHAR array)
!  * once an uncommented line is found, no other commented line is possible
!  * only 2 columns are read : x and y coordinates
!  * line width <= 999 (or change variables: line_fmt and line)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE

!     IN/OUT
      CHARACTER(LEN=*), INTENT(IN) :: filename
      TYPE(PT2D_SET), DIMENSION(:), INTENT(OUT), ALLOCATABLE :: pt_set_array
      INTEGER, INTENT(OUT) :: npoly

!     LOCAL VARIABLES
      CHARACTER(1), DIMENSION(2) :: COMMENT_CHAR = (/ '#', ':' /)
      CHARACTER(LEN=999) :: line
      CHARACTER(6) :: line_fmt = '(A999)'
      INTEGER :: ipoly, isom, nsom, I
      INTEGER, PARAMETER :: unit = 90

      npoly = i2s_NB_POLY(filename)
      WRITE(*,*) "Allocate pt_set_array with",npoly,"polylines"
      ALLOCATE(pt_set_array(npoly))

      OPEN(unit, file=TRIM(filename), status='old', action='read')

      DO ipoly = 1, npoly
 100    READ(unit,line_fmt) line

        ! Ignore lines starting by any comment character
        DO I = 1, SIZE(COMMENT_CHAR)
          IF(line(1:1).EQ.COMMENT_CHAR(I)) THEN
!            WRITE(*,*) 'Ignore a commented line'
            GOTO 100
          ENDIF
        ENDDO

        ! Read polyline caracteristics
        READ(line,*) nsom
        WRITE(*,*) 'Read polyline',ipoly,'with',nsom,'vertices'
        CALL ALLOC_PT2D_SET(nsom, pt_set_array(ipoly))

        ! Read x, y values
        DO isom = 1, nsom
          READ(unit,line_fmt) line
          READ(line,*) pt_set_array(ipoly)%x(isom), pt_set_array(ipoly)%y(isom)
        ENDDO

      ENDDO
      CLOSE(unit)

      END SUBROUTINE READ_i2s_PT2D_SET
!***********************************************************************
      INTEGER FUNCTION i2s_NB_POLY(filename)
!     @brief: return the number of polylines found in a i2s file
!     @info: stop if number of values is not coherant with the number of points/vertices
      IMPLICIT NONE
      CHARACTER(LEN=*) :: filename

      INTEGER, PARAMETER :: unit = 90
      CHARACTER(1), DIMENSION(2) :: COMMENT_CHAR = (/ '#', ':' /)
      CHARACTER(LEN=999) :: line
      CHARACTER(6) :: line_fmt = '(A999)'
      INTEGER :: I, nsom, tempo

      i2s_NB_POLY = 0

      OPEN(unit, file=TRIM(filename), status='old', action='read')

      ! Read file line by line...
 100  READ(unit,line_fmt,end=400) line
      ! Ignore commented lines
      DO I = 1, SIZE(COMMENT_CHAR)
        IF(line(1:1).EQ.COMMENT_CHAR(I)) THEN
          GOTO 100
        ENDIF
      ENDDO

      ! Read polyline caracteristics
      READ(line,*,end=400) nsom, tempo
      i2s_NB_POLY = i2s_NB_POLY + 1

      ! Ignore lines corresponding to the current polyline
      DO I = 1, nsom
        READ(unit,line_fmt,end=500) line
      ENDDO
      GOTO 100

 400  CLOSE(unit) ! Normal termination
      RETURN

 500  WRITE(*,*) "ERROR in function i2s_NB_POLY:"
      WRITE(*,*) "End of file reached while attempting to read points"
      WRITE(*,*) "polylign",i2s_NB_POLY,"seems uncomplete"
      WRITE(*,*) "or number of point incoherante:",nsom
      STOP

      END FUNCTION i2s_NB_POLY
!***********************************************************************
      SUBROUTINE PRINT_PT2D_SET(pt_set)
!     @brief: write coordinates for the user (usefull for debugging only?)
      IMPLICIT NONE
      TYPE(PT2D_SET), INTENT(IN) :: pt_set
      INTEGER :: isom
      DO isom = 1, pt_set%nsom
        WRITE(*,*) pt_set%x(isom), pt_set%y(isom)
      ENDDO
      END SUBROUTINE PRINT_PT2D_SET
!***********************************************************************
      END MODULE POLYLINES
