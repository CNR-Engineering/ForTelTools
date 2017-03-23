!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: horizontal interpolatation for mesh with triangular elements
! @info:
!   * table_node contains the three nodes of the element
!   * table_coeff the corresponding ponderation coefficients
!
! TODO:
!   * work with true interpolation functions (and not tables/matrixes)
!   * optimize with non homogenous shape
!       (combine interpolation functions with 3, 2, or 1 points)
!
! Contains subroutines/functions
!   * SUBROUTINE BUILD_INT_TABLES
!   * FUNCTION   INTERP_FROM_INT_TABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE INT_TABLES

      TYPE INTERP_DATA
        INTEGER :: nb_probes
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: Xprobe, Yprobe
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: table_node
        REAL*4, DIMENSION(:,:), ALLOCATABLE :: table_coeff
      END TYPE

      CONTAINS

!***********************************************************************
      SUBROUTINE BUILD_INT_TABLES(res, interp)
!     @brief: build interpolation tables from interp
!     @warnings: X, Y are REAL(8) but res%x and res%y are REAL(4)
      USE COMMON_DATA, ONLY: RESDATA, FIND_ELEMENT_FROM_POINT
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      TYPE(INTERP_DATA), INTENT(INOUT) :: interp

      DOUBLE PRECISION :: X, Y
      DOUBLE PRECISION, DIMENSION(res%ndp) :: XSOM, YSOM
      INTEGER :: I, elem
      DOUBLE PRECISION, DIMENSION(res%ndp) :: coeff ! tableau intermediaire pour avoir plus de precision pour les calculs... avant de passer en REAL*4 (un peu inutile sauf pour comparer?)

!      IF(res%ndp.NE.3) THEN
!        WRITE(*,*) "Problem ndp is not equal to 3"
!        WRITE(*,*) "Only triangle (linear) interpolation is programmed"
!        STOP
!      ENDIF

      ALLOCATE(interp%table_node(res%ndp,interp%nb_probes))
      ALLOCATE(interp%table_coeff(res%ndp,interp%nb_probes))

      DO I = 1, interp%nb_probes
        X = interp%Xprobe(I)
        Y = interp%Yprobe(I)

        elem = FIND_ELEMENT_FROM_POINT(res, X, Y)

        IF(elem.GT.0) THEN ! Point is located inside element n°elem
          ! Get X and Y of nodes which constitues the element elem
          interp%table_node(:,I) = res%ikle(:,elem)
          XSOM = res%x(interp%table_node(:,I))
          YSOM = res%y(interp%table_node(:,I))

          ! Compute coeff with barycentric coordinate system in a triangle
          coeff = ABS(
     &     (CSHIFT(XSOM,SHIFT=1)-X)*(CSHIFT(YSOM,SHIFT=2)-Y) -
     &     (CSHIFT(XSOM,SHIFT=2)-X)*(CSHIFT(YSOM,SHIFT=1)-Y)
     &    )

          ! Compute dimensionless coefficients
          coeff = coeff/SUM(coeff)
        ELSE ! Point is outside the domain
          ! No interpolation, only the value of the closest point
          interp%table_node(:,I)  = (/ -elem,    1,    1 /)
          ! node number 1 is not used (ponderation coeff is set to 0)
          !   but avoids a segmentation fault.
          coeff = (/  1.D0, 0.D0, 0.D0 /)
        ENDIF
        interp%table_coeff(:,I) = REAL(coeff)
      ENDDO

      END SUBROUTINE BUILD_INT_TABLES
!***********************************************************************
      FUNCTION INTERP_FROM_INT_TABLES(val, interp) RESULT(res)
!     @brief: compute interpolated values from interpolation tables
      IMPLICIT NONE
      REAL*4, DIMENSION(:), INTENT(IN) :: val
      TYPE(INTERP_DATA), INTENT(IN) :: interp
      REAL*4, DIMENSION(interp%nb_probes) :: res

      INTEGER :: I

      DO I = 1, interp%nb_probes
        res(I)=SUM(val(interp%table_node(:,I))*interp%table_coeff(:,I))
      ENDDO

      END FUNCTION INTERP_FROM_INT_TABLES

      END MODULE INT_TABLES
