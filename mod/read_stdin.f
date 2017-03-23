!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: read and return values from stdin
!
! Contains subroutines/functions:
!   * FUNCTION   YES_OR_NO
!   * FUNCTION   STDIN_INT
!   * FUNCTION   STDIN_REAL
!   * FUNCTION   STDIN_DBLE
!   * SUBROUTINE STDIN_ARRAY_INT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE READ_STDIN

      INTEGER, PARAMETER :: inunit = 5

      CHARACTER*80 :: buffer
      CHARACTER*5, PARAMETER :: fmt_buffer = '(A80)'

      CHARACTER*3 :: start_question = '-> '

      CONTAINS
!***********************************************************************
      CHARACTER*80 FUNCTION READ_ANSWER_STR(text, default) !FIXME: auto-length?
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: text
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: default

      IF(PRESENT(default)) THEN
        WRITE(*,*) start_question//text//' ['//TRIM(default)//']'
      ELSE
        WRITE(*,*) start_question//text
      ENDIF

      READ(inunit,fmt_buffer) READ_ANSWER_STR

      IF(PRESENT(default).AND.READ_ANSWER_STR.EQ.'') THEN
        READ_ANSWER_STR = default
      ENDIF

      END FUNCTION READ_ANSWER_STR
!***********************************************************************
      CHARACTER*80 FUNCTION READ_ANSWER_PATH(path, inname, preffix) !FIXME: auto-length?
!     @brief: like READ_ANSWER_STR but with a default preffix integrated
      USE COMMON_DATA, ONLY: STR_ADD_SUFFIX
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: path, preffix, inname
      CHARACTER(LEN=256) :: path_default !FIXME: arbitrary length

      path_default = inname
      CALL STR_ADD_SUFFIX(path_default, preffix)

      READ_ANSWER_PATH = READ_ANSWER_STR(path, path_default)

      END FUNCTION READ_ANSWER_PATH
!***********************************************************************
      LOGICAL FUNCTION YES_OR_NO(text, default)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: text
      LOGICAL, INTENT(IN), OPTIONAL :: default
      CHARACTER*1 :: in

      IF(PRESENT(default)) THEN
        IF(default) THEN
          WRITE(*,*) start_question//TRIM(text)//' ([y]/n)'
        ELSE
          WRITE(*,*) start_question//TRIM(text)//' (y/[n])'
        ENDIF
      ENDIF

 50   READ(inunit,'(A1)') in

      IF(in.EQ.'Y'.OR.in.EQ.'y') THEN
        YES_OR_NO = .TRUE.
      ELSE IF(in.EQ.'N'.OR.in.EQ.'n') THEN
        YES_OR_NO = .FALSE.
      ELSE IF(in.EQ.' '.AND.PRESENT(default)) THEN
        YES_OR_NO = default
      ELSE
        GOTO 50
!        WRITE(*,*) "Value is not allowed"
!        STOP
      ENDIF

      END FUNCTION YES_OR_NO
!***********************************************************************
      INTEGER FUNCTION STDIN_INT(text, default)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: text
      INTEGER, INTENT(IN), OPTIONAL :: default

      WRITE(*,*) start_question//TRIM(text)

      IF(present(default)) THEN
        ! Read buffer as string and convert it if not empty
        READ(inunit,fmt_buffer) buffer
        IF(buffer.EQ.' ') THEN
          STDIN_INT = default
        ELSE
          READ(buffer,*) STDIN_INT
        ENDIF
      ELSE
        READ(inunit,*) STDIN_INT
      ENDIF

      END FUNCTION STDIN_INT
!***********************************************************************
      REAL FUNCTION STDIN_REAL(text, default, default_display)
!     @brief: return a real from stdin answer
!       A default value is optional and a default_display can then be
!         precised to write plain text instead of default as real
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: text
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: default_display
      REAL*4, INTENT(IN), OPTIONAL :: default
      CHARACTER(LEN=10) :: default_str ! consistent with string to real conversion

      IF(PRESENT(default).AND.PRESENT(default_display)) THEN
!       default_display is diplayed and should be consistent with the default value
        WRITE(*,*) start_question//TRIM(text)//' ['//default_display//']'
      ELSE IF(PRESENT(default)) THEN
!       Convert default as string and display it in the question
        WRITE(default_str,'(f10.2)') default
        WRITE(*,*) start_question//TRIM(text)//' ['//TRIM(ADJUSTL(default_str))//']'
      ELSE
!       Question without default value
        WRITE(*,*) start_question//TRIM(text)
      ENDIF

 50   READ(inunit,fmt_buffer) buffer

      IF(buffer.EQ.' ') THEN
!       No value entered by the user
        IF(PRESENT(default)) THEN
          STDIN_REAL = default
        ELSE
!         No default value, a real is compulsory => repeat question
          GOTO 50
        ENDIF
      ELSE
!       buffer is converted to real
        READ(buffer,*) STDIN_REAL
      ENDIF

      END FUNCTION STDIN_REAL
!***********************************************************************
      DOUBLE PRECISION FUNCTION STDIN_DBLE(text, default, default_display)
!     @brief: return a double precision from stdin answer
!       A default value is optional and a default_display can then be
!         precised to write plain text instead of default as real
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: text
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: default_display
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: default
      CHARACTER(LEN=10) :: default_str ! consistent with string to real conversion

      IF(PRESENT(default).AND.PRESENT(default_display)) THEN
!       default_display is diplayed and should be consistent with the default value
        WRITE(*,*) start_question//TRIM(text)//' ['//default_display//']'
      ELSE IF(PRESENT(default)) THEN
!       Convert default as string and display it in the question
        WRITE(default_str,'(d10.2)') default
        WRITE(*,*) start_question//TRIM(text)//' ['//TRIM(ADJUSTL(default_str))//']'
      ELSE
!       Question without default value
        WRITE(*,*) start_question//TRIM(text)
      ENDIF

 50   READ(inunit,fmt_buffer) buffer

      IF(buffer.EQ.' ') THEN
!       No value entered by the user
        IF(PRESENT(default)) THEN
          STDIN_DBLE = default
        ELSE
!         No default value, a real is compulsory => repeat question
          GOTO 50
        ENDIF
      ELSE
!       buffer is converted to real
        READ(buffer,*) STDIN_DBLE
      ENDIF

      END FUNCTION STDIN_DBLE
!***********************************************************************
      SUBROUTINE STDIN_ARRAY_INT(max_size, stop_value, int_array)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: max_size
      INTEGER, INTENT(IN) :: stop_value
      INTEGER, DIMENSION(:), INTENT(OUT), ALLOCATABLE :: int_array

      INTEGER :: int_read
      INTEGER :: I, J
      INTEGER, DIMENSION(max_size) :: temp

      I = 0
 70   READ(inunit,*) int_read
      IF(int_read.NE.stop_value) THEN
        I = I + 1
        temp(I) = int_read
        GOTO 70
      ENDIF

      ALLOCATE(int_array(I))
      DO J = 1, I
        int_array(J) = temp(J)
      ENDDO

      END SUBROUTINE STDIN_ARRAY_INT
!***********************************************************************
      END MODULE READ_STDIN
