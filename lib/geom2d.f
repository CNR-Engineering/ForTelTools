!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: tools for 2D geometry
!
! CONTAINS
! * FUNCTION   DISTANCE_LINE
! * FUNCTION   DISTANCE_SEGMENT
! * FUNCTION   DISTANCE_POLYLINE
! * FUNCTION   RAD2DEG
! * FUNCTION   DEG2RAD
! * SUBROUTINE ROTATION
! * SUBROUTINE HOMOTHETY
!
! TODO: general type: real or double precision
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE GEOM2D

      REAL, PARAMETER :: PI = REAL(4.D0*DATAN(1.D0))

      CONTAINS
!
!-----------------------------------------------------------------------
!     GEOMETRIC DISTANCES
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION FUNCTION DISTANCE_LINE(AX, AY, BX, BY, QX, QY)
!     @brief: Compute distance from a point Q to an infinite line AB
!     @prerequisites: points A and B are different (ie length AB is not equal to 0)
      IMPLICIT NONE
      DOUBLE PRECISION :: AX, AY, BX, BY, QX, QY

      DISTANCE_LINE = (AX-BX)*(BY-QY) - (AY-BY)*(BX-QX)
      DISTANCE_LINE = ABS(DISTANCE_LINE)/SQRT((AX-BX)**2+(AY-BY)**2)

      RETURN
      END FUNCTION DISTANCE_LINE
!***********************************************************************
      DOUBLE PRECISION FUNCTION DISTANCE_SEGMENT(AX, AY, BX, BY, QX, QY)
!     @brief: Compute distance from a point Q to a segment AB
!     @dependencies: FUNCTION DISTANCE_LINE
      IMPLICIT NONE
      DOUBLE PRECISION :: AX, AY, BX, BY, QX, QY
      DOUBLE PRECISION :: PROD_SCAL, LONG_LINE_2

      LONG_LINE_2 = (AX-BX)**2+(AY-BY)**2 ! square of AB distance

      ! 3 cases depending if Q is inside or outside the domain formed by
      ! two perpendicular lines of AB passing respectively by A and B.
      ! => Compute dot product between AB and AQ
      PROD_SCAL = (BX-AX)*(QX-AX)+(QY-AY)*(BY-AY)

      IF(PROD_SCAL.LT.0.D0) THEN
        ! OUTSIDE, SIDE OF A => RETURN AQ
        DISTANCE_SEGMENT = SQRT((AX-QX)**2+(AY-QY)**2)
      ELSE IF(PROD_SCAL.GT.LONG_LINE_2) THEN
        ! OUTSIDE, SIDE OF B => RETURN BQ
        DISTANCE_SEGMENT = SQRT((BX-QX)**2+(BY-QY)**2)
      ELSE
        ! INSIDE => RETURN DISTANCE_LINE AB to Q
        DISTANCE_SEGMENT = DISTANCE_LINE(AX, AY, BX, BY, QX, QY)
      ENDIF

      RETURN
      END FUNCTION DISTANCE_SEGMENT
!***********************************************************************
      DOUBLE PRECISION FUNCTION DISTANCE_POLYLINE(QX, QY, NSOM, XSOM, YSOM)
!     @brief: Compute distance from a point Q to a polyline
!     @info: number of vertices is equal to number of segment
!        (polyline is closed without duplication of any vertices)
!     @dependencies: FUNCTION DISTANCE_SEGMENT
!     @prerequisites: -
      IMPLICIT NONE
      DOUBLE PRECISION :: AX, AY, BX, BY, QX, QY, DISTANCE
      INTEGER :: ISEG, NSOM
      DOUBLE PRECISION, DIMENSION(NSOM) :: XSOM, YSOM

      DO ISEG = 1, NSOM
        ! Define segment AB
        AX = XSOM(ISEG)
        AY = YSOM(ISEG)
        IF(ISEG.EQ.NSOM) THEN
          BX = XSOM(1)
          BY = YSOM(1)
        ELSE
          BX = XSOM(ISEG+1)
          BY = YSOM(ISEG+1)
        ENDIF

        ! Compute distance from AB to Q
        DISTANCE = DISTANCE_SEGMENT(AX, AY, BX, BY, QX, QY)
        IF(ISEG.EQ.1) THEN
          DISTANCE_POLYLINE = DISTANCE
        ELSE
          DISTANCE_POLYLINE = MIN(DISTANCE_POLYLINE, DISTANCE)
        ENDIF
      ENDDO

      RETURN
      END FUNCTION DISTANCE_POLYLINE
!
!-----------------------------------------------------------------------
!     SURFACE
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION FUNCTION SURFACE_POLY(nsom, X, Y)
!     @brief: compute surface of a simple polylign (not closed in "a eight")
!       The polygone is closed (WITHOUT duplication of point)
      IMPLICIT NONE
      INTEGER :: nsom
      DOUBLE PRECISION, DIMENSION(nsom) :: X, Y
      INTEGER :: isom

!     Start with a specific value: first and last points to close polygon
      SURFACE_POLY = X(nsom)*Y(1) - X(1)*Y(nsom)
      DO isom = 1, nsom-1
        SURFACE_POLY = SURFACE_POLY + X(isom)*Y(isom+1) - X(isom+1)*Y(isom)
      ENDDO

!     SURFACE_POLY can be negative here according the order of the vertices
      SURFACE_POLY = ABS(SURFACE_POLY)/2.D0

      END FUNCTION SURFACE_POLY
!
!-----------------------------------------------------------------------
!     GEOMETRIC TRANSFORMATIONS
!-----------------------------------------------------------------------
!
      REAL FUNCTION RAD2DEG(angle_rad)
!     @brief: convert radians to degrees
      IMPLICIT NONE
      REAL :: angle_rad
      RAD2DEG = angle_rad*180.0/PI
      END FUNCTION RAD2DEG
!***********************************************************************
      REAL FUNCTION DEG2RAD(angle_deg)
!     @brief: convert degrees to radians
      IMPLICIT NONE
      REAL :: angle_deg
      DEG2RAD = angle_deg*PI/180.0
      END FUNCTION DEG2RAD
!***********************************************************************
      SUBROUTINE ROTATION(nsom, Xin, Yin, Xout, Yout, Xc, Yc, theta_deg)
!     @brief: compute transformed coordinates obtained by rotation
!     @param Xin, Yin, Xout, Yout <dble array>: list of input and output coordinates
!     @param Xc, Yc <dble>: X and Y rotation center coordinates
!     @param theta_deg <dble>: rotation angle in degree (anticlockwise sense)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: nsom
      REAL, INTENT(IN),  DIMENSION(nsom) :: Xin, Yin
      REAL, INTENT(OUT), DIMENSION(nsom) :: Xout, Yout
      REAL, INTENT(IN) :: Xc, Yc, theta_deg
      REAL :: theta_rad

      theta_rad = DEG2RAD(theta_deg)
      Xout(:) = Xc + (Xin(:)-Xc)*COS(theta_rad) - (Yin(:)-Yc)*SIN(theta_rad)
      Yout(:) = Yc + (Xin(:)-Xc)*SIN(theta_rad) + (Yin(:)-Yc)*COS(theta_rad)

      END SUBROUTINE ROTATION
!***********************************************************************
      SUBROUTINE HOMOTHETY(nsom, Xin, Yin, Xout, Yout, Xc, Yc, ratio)
!     @brief: compute transformed coordinates obtained by homothety
!     @prerequisites: ratio different from zero...
!     @param nsom <int>: number of points (ie size of Xin, Yin, Xout, Yout)
!     @param Xin, Yin, Xout, Yout <dble array>: list of input and output coordinates
!     @param Xc, Yc <dble>: X and Y center coordinates
!     @param ratio <dble>: transformation ratio (>1 for enlargement)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: nsom
      REAL, INTENT(IN),  DIMENSION(nsom) :: Xin, Yin
      REAL, INTENT(OUT), DIMENSION(nsom) :: Xout, Yout
      REAL, INTENT(IN) :: Xc, Yc, ratio

      Xout(:) = ratio*( Xin(:) - Xc ) + Xc
      Yout(:) = ratio*( Yin(:) - Yc ) + Yc

      END SUBROUTINE HOMOTHETY
!***********************************************************************
      END MODULE GEOM2D
