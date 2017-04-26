!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: handle result files from hydro-dynamics simulation
!
! Contains subroutines/functions
!   * SUBROUTINE CLOSE_RES
!   * SUBROUTINE COPY_HEADER (cannot be a function!)
!   * SUBROUTINE VARS_TO_KEEP (function is buggy with alloc with ifort)
!   * SUBROUTINE ASSIGN_VARS
!   *(SUBROUTINE PRINT_VARS)
!   * SUBROUTINE PRINT_HEADER

!   * FUNCTION   NEAREST_NODE
!   * FUNCTION   FIND_ELEMENT_FROM_POINT
!   * FUNCTION   SURF_ELEM
!   * FUNCTION   FIND_VAR_POS

!   * SUBROUTINE CHECK_MESHTYPE
!   * SUBROUTINE CHECK_NDP

!   * SUBROUTINE STR_REPLACE_CHARS
!   * FUNCTION   STR_ADD_SUFFIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE COMMON_DATA

      INTEGER, PARAMETER :: line_length  = 78

      TYPE RESDATA
        CHARACTER*80 :: filename
        INTEGER      :: id

!       Attributes for Serafin export
        CHARACTER*80 :: title
        INTEGER      :: nbvar, nbvar2
        CHARACTER*32, DIMENSION(:), ALLOCATABLE :: varnames
        INTEGER, DIMENSION(10) :: param
        INTEGER      :: nplan
        INTEGER      :: nelem, nnode, ndp, i
        INTEGER, DIMENSION(6)  :: date
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ikle
        INTEGER, DIMENSION(:),   ALLOCATABLE :: ipobo
        REAL*4,  DIMENSION(:),   ALLOCATABLE :: x, y

!       Other attributes to improve handling of RESDATA object
        INTEGER :: nnode2d
        CHARACTER*2 :: meshtype
      END TYPE

      PRIVATE :: PRINT_VARS

      CONTAINS

!***********************************************************************
      SUBROUTINE CLOSE_RES(res, deallocate_res)
!     @brief: close file and deallocate if asked
!     (deallocate only if deallocate_res is present and .TRUE.)
      IMPLICIT NONE
      TYPE(RESDATA) :: res
      LOGICAL, INTENT(IN), OPTIONAL :: deallocate_res

      IF(PRESENT(deallocate_res)) THEN
        IF(deallocate_res) THEN
          DEALLOCATE(res%varnames)
          DEALLOCATE(res%ikle)
          DEALLOCATE(res%ipobo)
          DEALLOCATE(res%x)
          DEALLOCATE(res%y)
        ENDIF
      ENDIF
!      WRITE(*,*) 'Close connection',res%id
      CLOSE(res%id)
      END SUBROUTINE CLOSE_RES
!***********************************************************************
      SUBROUTINE COPY_HEADER(resin, resout)
!     @brief: Copy resin in resout but keep the id (file connection) and filename
!     @param resin <RESDATA>: object to copy
!     @param resout <RESDATA>: the modified copy
!     @warnings: resout%id and resout%filename should exist, but the arrays are not allocated
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN)    :: resin
      TYPE(RESDATA), INTENT(INOUT) :: resout

      INTEGER      :: id
      CHARACTER*80 :: filename

      ! Copy all except the id and filename
      id = resout%id
      filename = resout%filename

      ! Allocate arrays
      ! This step is not compulsory for all compilers
      !   (because the next assignement can do an automatic allocation)
      ALLOCATE(resout%varnames(resin%nbvar))
      ALLOCATE(resout%ikle(resin%ndp,resin%nelem))
      ALLOCATE(resout%ipobo(resin%nnode))
      ALLOCATE(resout%x(resin%nnode))
      ALLOCATE(resout%y(resin%nnode))
      resout = resin

      ! Overwrite by original values
      resout%id = id
      resout%filename = filename

      END SUBROUTINE COPY_HEADER
!***********************************************************************
      SUBROUTINE VARS_TO_KEEP(res, var2del, var2keep)
!     @brief: assign new variables
!     @param res <RESDATA>: object to modify
!     @param var2del <int array>: list of variables to remove. Ex: (/1,3/)
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      INTEGER, DIMENSION(:), INTENT(IN)  :: var2del
      INTEGER, DIMENSION(:), INTENT(OUT), ALLOCATABLE :: var2keep

      INTEGER :: I, J

      ALLOCATE(var2keep(res%nbvar - SIZE(var2del)))
      J = 1
      DO I = 1, res%nbvar
        IF(ALL(var2del.NE.I)) THEN
          var2keep(J) = I
          J = J + 1
        ENDIF
      ENDDO

      END SUBROUTINE VARS_TO_KEEP
!***********************************************************************
      SUBROUTINE ASSIGN_VARS(resin, resout, var2keep)
!     @brief: assign some variables (var2keep) from resin to resout
!     @param resin <RESDATA>: object to copy
!     @param resout <RESDATA>: the modified copy
!     @param var2keep <int array>: list of variables to keep. Ex: (/ 1, 5, 3 /)
!     /!\ ORDER IN VAR2KEEP IS ESSENTIAL
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN)    :: resin
      TYPE(RESDATA), INTENT(INOUT) :: resout
      INTEGER, DIMENSION(:) :: var2keep
      INTEGER :: I

      ! Check consistency of elements in var2keep
      IF(ANY(var2keep.LT.1).OR.ANY(var2keep.GT.resin%nbvar)) THEN
        WRITE(*,*) "Error some variable(s) could not be deleted"
        STOP
      ENDIF

      ! Build a new sized resout%varnames array
      DEALLOCATE(resout%varnames)
      resout%nbvar = SIZE(var2keep)
      ALLOCATE(resout%varnames(resout%nbvar))
      DO I = 1, resout%nbvar
        resout%varnames(I) = resin%varnames(var2keep(I))
      ENDDO

      END SUBROUTINE ASSIGN_VARS
!***********************************************************************
      SUBROUTINE PRINT_VARS(varnames)
!     @brief: print list of variables (with their rank)
      IMPLICIT NONE
      CHARACTER*32, DIMENSION(:) :: varnames
      INTEGER :: I
      DO I = 1, SIZE(varnames)
        WRITE(*,*) I,'"',varnames(I),'"'
      ENDDO
      END SUBROUTINE PRINT_VARS
!***********************************************************************
      SUBROUTINE PRINT_HEADER(res)
!     @brief: print informations about the file/mesh
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res

      WRITE(*,*) repeat('*', line_length)
      WRITE(*,*) "> Title of the study       :"
      WRITE(*,*) '"',res%title,'"'
      WRITE(*,*) "> Number of elements       :",res%nelem
      WRITE(*,*) "> Number of 2D nodes       :",res%nnode2d
      IF(res%meshtype.EQ.'3D') THEN
        WRITE(*,*) "> Number of planes         :",res%nplan
        WRITE(*,*) "> Number of 3D nodes       :",res%nnode
      ENDIF
      WRITE(*,*) "> Number of variables      :",res%nbvar
      WRITE(*,*) "> Name of stored variables :"
      CALL PRINT_VARS(res%varnames)
      WRITE(*,*) repeat('*', line_length)

      END SUBROUTINE PRINT_HEADER
!
!=======================================================================
!     FIND INFORMATIONS FROM MESH
!=======================================================================
!
      INTEGER FUNCTION NEAREST_NODE(res, X, Y)
!     @brief: nearest node from point with (X,Y) coordinates
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      DOUBLE PRECISION, INTENT(IN) :: X, Y

      NEAREST_NODE = MINLOC(SQRT((res%x-X)**2 + (res%y-Y)**2), 1)

      END FUNCTION NEAREST_NODE
!***********************************************************************
      INTEGER FUNCTION FIND_ELEMENT_FROM_POINT(res, X, Y)
!     @brief: return element number in which the target point is
!     if the point is outside the domain, then return the nearest node as a negative integer
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      DOUBLE PRECISION, INTENT(IN) :: X, Y
      LOGICAL :: INPOLY ! function dependance

      INTEGER, PARAMETER :: nb_elt = 3
      INTEGER, DIMENSION(nb_elt) :: nodes
      INTEGER :: CL_node, I

      CL_node = NEAREST_NODE(res, X, Y)
      FIND_ELEMENT_FROM_POINT = -CL_node
      DO I = 1, res%nelem
        nodes = res%ikle(:,I)
        IF(ANY(nodes.EQ.CL_node)) THEN
          IF(INPOLY(X, Y, DBLE(res%x(nodes)), DBLE(res%y(nodes)), nb_elt)) THEN
            FIND_ELEMENT_FROM_POINT = I
            EXIT ! stop do loop
          ENDIF
        ENDIF
      ENDDO

      END FUNCTION FIND_ELEMENT_FROM_POINT
!***********************************************************************
      FUNCTION SURF_ELEM(res)
!     @brief: return an array with surface (projection on horizontal)
!       of each elements
!     FIXME: only triangular element and 2D????
      USE GEOM2D, ONLY: SURFACE_POLY
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      DOUBLE PRECISION, DIMENSION(res%nelem) :: SURF_ELEM

      INTEGER :: ielem
      INTEGER, PARAMETER :: ndp = 3
      INTEGER, DIMENSION(ndp) :: nodes

      DO ielem = 1, res%nelem
        nodes = res%ikle(:,ielem)
        SURF_ELEM(ielem) = SURFACE_POLY(ndp, DBLE(res%x(nodes)), DBLE(res%y(nodes))) !FIXME: REAL, DOUBLE...
      ENDDO

      END FUNCTION SURF_ELEM
!***********************************************************************
      INTEGER FUNCTION FIND_VAR_POS(text, res)
!     @brief: find variable rank with the beginning of the variable name
      IMPLICIT NONE
      CHARACTER(LEN=*) :: text
      TYPE(RESDATA) :: res
      INTEGER :: I

      FIND_VAR_POS = 0
      DO I = 1, res%nbvar
        IF(res%varnames(I)(1:LEN_TRIM(text)).EQ.text) THEN
          FIND_VAR_POS = I
          EXIT
        ENDIF
      ENDDO

      END FUNCTION
!
!=======================================================================
!     CHECK CONSISTENCY OF VARIABLES AND STOP/WARN IF NECESSARY
!=======================================================================
!
      SUBROUTINE CHECK_MESHTYPE(res, target_type)
!     @brief: check the meshtype and stop with an error message
!       if target_type does not correspond to the meshtype in res
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      CHARACTER*2, INTENT(IN) :: target_type

      IF(res%meshtype.NE.target_type) THEN
        WRITE(*,*) "The mesh type of the file ",TRIM(res%filename)," is not the one intented"
        WRITE(*,*) "For this program, the type has to be: ",target_type,"and not",res%meshtype
        STOP
      ENDIF

      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CHECK_NDP(res, target_ndp)
!     @brief: check ndp
      IMPLICIT NONE
      TYPE(RESDATA), INTENT(IN) :: res
      INTEGER, INTENT(IN) :: target_ndp

      IF(res%ndp.NE.target_ndp) THEN
        WRITE(*,*) res%ndp," is not a correct ndp value:"
        WRITE(*,*) target_ndp," was expected."
        STOP
      ENDIF

      END SUBROUTINE
!
!=======================================================================
!     MANIPULATE STRING
!=======================================================================
!
      SUBROUTINE STR_REPLACE_CHARS(string, char2replace, char)
!     @brief: replaces multiple characters (from char2replace)
!       by a single char in a string
!     @warnings: only the LEN_TRIM first characters of the string are
!       replaced. Keep in mind to use TRIM afterwards if necessary.
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: string
      CHARACTER(LEN=1), DIMENSION(:), INTENT(IN) :: char2replace
      CHARACTER(LEN=1), INTENT(IN) :: char
      INTEGER :: I, J

      DO I = 1, LEN_TRIM(string)
        DO J = 1, SIZE(char2replace)
          IF(string(I:I).EQ.char2replace(J)) THEN !FIXME: ICHAR nécessaire?
            string(I:I) = char
          ENDIF
        ENDDO
      ENDDO

      END SUBROUTINE STR_REPLACE_CHARS
!***********************************************************************
      SUBROUTINE STR_ADD_SUFFIX(path, suffix)
!     @brief: add suffix in path (before extension if present)
!     @warnings: only linux separator
!       (folder separator is '/' and not '\' or '\\')
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: path
      CHARACTER(LEN=*), INTENT(IN) :: suffix

      CHARACTER(LEN=256) :: base, ext
      INTEGER pos_point, pos_sep
      LOGICAL has_ext

      pos_point = INDEX(path, '.', BACK=.TRUE.)
      pos_sep = INDEX(path, '/', BACK=.TRUE.)

      IF(pos_point.GT.pos_sep) THEN
        has_ext = .TRUE.
      ELSE
        has_ext = .FALSE.
      ENDIF

      IF(has_ext) THEN
        base = path(1:pos_point-1)
        ext = path(pos_point+1:LEN_TRIM(path))
        path = TRIM(base)//TRIM(suffix)//'.'//TRIM(ext)
      ELSE
        base = path
        path = TRIM(base)//TRIM(suffix)
      ENDIF

      END SUBROUTINE STR_ADD_SUFFIX
!***********************************************************************
      FUNCTION LJUST(instr, len) RESULT(outstr)
!     @brief: return a string of length len corresponding to instr with
!       trailing whitespaces (behaves like ljust in Python)
!     @problem: string may not contain accents (bug with gfortran)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: instr
      INTEGER, INTENT(IN) :: len
      CHARACTER(LEN=len) :: outstr

      IF(LEN_TRIM(instr).GT.len) THEN
        WRITE(*,*) "instr is too long! It can not be filled by whitespaces"
        STOP
      ENDIF
      outstr = repeat(' ', len)
      outstr(1:LEN_TRIM(instr)) = TRIM(instr)

      END FUNCTION LJUST
!***********************************************************************
      SUBROUTINE SCRIPT_HEADER(name, version, brief)
!     @brief: display a personalized program header
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: name, version, brief

      WRITE(*,*)
      WRITE(*,*) repeat('#', line_length)
      WRITE(*,*) "#                                                                            #"
      WRITE(*,*) "#  Programme : "// LJUST(name, 60)    //"  #"
      WRITE(*,*) "#  Version   : "// LJUST(version, 60) //"  #"
      WRITE(*,*) "#  "//             LJUST(brief, 72)   //"  #"
      WRITE(*,*) "#                                                                            #"
      WRITE(*,*) repeat('#', line_length)
      WRITE(*,*)

      END SUBROUTINE
!***********************************************************************
      END MODULE COMMON_DATA
