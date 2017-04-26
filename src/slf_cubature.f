!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: integrate a variable over the vertical
! @info:
!   * integration of WATER DEEPTH returns the volume of water
!       (without correction for tidal flats)
!   * integration of BOTTOM gives the volume of erosion/deposition
!       (with initialisation with first frame)
!   * global balance is always computed
!   * possibility to define zones (with a polylines file)
! @TODO: write in CSV format (comma separator)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_CUBATURE
      USE READ_STDIN, ONLY: READ_ANSWER_STR, YES_OR_NO, STDIN_INT
      USE COMMON_DATA, ONLY: SCRIPT_HEADER, RESDATA, SURF_ELEM,
     &  PRINT_HEADER, CLOSE_RES, CHECK_NDP
      USE SERAFIN
      USE POLYLINES, ONLY: PT2D_SET, READ_i2s_PT2D_SET
      IMPLICIT NONE

      ! Variables defined by the user
      CHARACTER*80 :: inname, outname, polyname
      LOGICAL :: init, user_zones
      INTEGER :: pos_var

      LOGICAL :: INPOLY ! function dependance
      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51
      INTEGER, PARAMETER :: polyunit = 52

      TYPE(RESDATA) :: resin
      LOGICAL :: eof
      REAL*4 :: time
      REAL*4, DIMENSION(:), ALLOCATABLE  :: init_value
      INTEGER :: IFRAME, I, J, node, ipoly, npoly, inode, nnode_inside, nzone
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: values
      REAL*4, DIMENSION(:), ALLOCATABLE :: surface_elem
      TYPE(PT2D_SET) :: cur_pt_set
      TYPE(PT2D_SET), DIMENSION(:), ALLOCATABLE :: pt_set_array
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: coeff
      LOGICAL, DIMENSION(:), ALLOCATABLE :: node_inpoly
      REAL*4, DIMENSION(:), ALLOCATABLE :: val2write

      CALL SCRIPT_HEADER("slf_cubature", "1.0",
     &  "Calculer evolution temporelle de volume")

      ! Get filenames and open file connections
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)

      outname = READ_ANSWER_STR("Output CSV file")
      OPEN(outunit, file=TRIM(outname), status='new')

      CALL SLF_READ_HEADER(resin)
      CALL CHECK_NDP(resin, 3)
      CALL PRINT_HEADER(resin)

      pos_var = STDIN_INT("Index/rank of the variable")
      IF(pos_var.LT.1.OR.pos_var.GT.resin%nbvar) THEN
        WRITE(*,*) pos_var,"is not a correct rank"
        WRITE(*,*) "It shoubd be between",1,"and",resin%nbvar
        STOP
      ENDIF

      ALLOCATE(surface_elem(resin%nelem))

!     Compute surface of each elements
      surface_elem = REAL(SURF_ELEM(resin))

      init = YES_OR_NO("Initialize to zero with first frame values ?", .FALSE.)

      user_zones = YES_OR_NO("Use user defined zones ?", .FALSE.)

      nzone = 1
      IF(user_zones) THEN
        polyname = READ_ANSWER_STR("Filename for i2s file:")
        WRITE(*,*) "READING i2s file..."
        CALL READ_i2s_PT2D_SET(polyname, pt_set_array, npoly)
        nzone = nzone + npoly
      ENDIF

      ALLOCATE(node_inpoly(resin%nnode))
      ALLOCATE(coeff(resin%nnode,nzone))
      coeff(:,:) = 0.E0

      WRITE(*,*) "Compute coefficient for each zone"
      DO ipoly = 1, nzone
        IF(ipoly.NE.nzone) THEN
          cur_pt_set = pt_set_array(ipoly)
!         1) Find node inside current polyline
          DO inode = 1, resin%nnode
            node_inpoly(inode) = INPOLY(DBLE(resin%x(inode)), DBLE(resin%y(inode)),
     &        DBLE(cur_pt_set%x), DBLE(cur_pt_set%y), cur_pt_set%nsom)
          ENDDO
          nnode_inside = COUNT(node_inpoly(:))
        ELSE ! Whole domain
          node_inpoly(:) = .TRUE.
          nnode_inside = resin%nnode
        ENDIF

!       2) Compute coefficients of array values
!          For each node, the ponderation is equal sum of the horizontal
!            surface of linked elements divided by three
        DO I = 1, resin%nelem
          DO J = 1, resin%ndp
            node = resin%ikle(J,I)
            IF(node_inpoly(node)) THEN
              coeff(node,ipoly) = coeff(node,ipoly) + surface_elem(I)
            ENDIF
          ENDDO
        ENDDO
        coeff(:,:) = coeff(:,:)/3.E0

        WRITE(*,*) "Polyline",ipoly,"contains",nnode_inside,"nodes for a surface:",SUM(coeff(:,ipoly))
      ENDDO

!     Prepare temporal reading
      DEALLOCATE(surface_elem)
      DEALLOCATE(node_inpoly)
      ALLOCATE(values(resin%nnode,1))
      ALLOCATE(val2write(nzone))
      ALLOCATE(init_value(nzone))
      init_value(:) = 0.E0 ! default value without initialization to zero
      IFRAME = 0

!     Time loop
 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof) GOTO 90
      values = SLF_READ_VARS_FRAME(resin, (/ pos_var /))
      IFRAME = IFRAME + 1
      IF(init) THEN
        IF(IFRAME.EQ.1) THEN
          DO ipoly = 1, nzone
            init_value(ipoly) = DOT_PRODUCT(values(:,1), coeff(:,ipoly))
          ENDDO
        ENDIF
      ENDIF
      DO ipoly = 1, nzone
        val2write(ipoly) = DOT_PRODUCT(values(:,1), coeff(:,ipoly)) - init_value(ipoly)
      ENDDO
      WRITE(outunit,*) time, val2write(:)
      GOTO 80

 90   CALL CLOSE_RES(resin)
      CLOSE(outunit)

      END PROGRAM SLF_CUBATURE
