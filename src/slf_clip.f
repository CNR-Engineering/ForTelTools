!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! @brief: clip values above/below a threshold
! @info: only stricly (ie not equal) above/below
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PROGRAM SLF_CLIP
      USE READ_STDIN, ONLY: READ_ANSWER_STR, READ_ANSWER_PATH,
     &  YES_OR_NO, STDIN_INT, STDIN_REAL, STDIN_ARRAY_INT
      USE COMMON_DATA
      USE SERAFIN
      IMPLICIT NONE

!     Variables defined by the user
      CHARACTER*80 :: inname, outname
      INTEGER :: pos_ref
      INTEGER, DIMENSION(:), ALLOCATABLE :: var2clip
      LOGICAL :: clip_min, clip_max
      REAL*4 :: min_val, max_val, min_thd, max_thd

!     Local variables
      INTEGER, PARAMETER :: inunit  = 50
      INTEGER, PARAMETER :: outunit = 51
      TYPE(RESDATA) :: resin, resout
      LOGICAL       :: eof
      REAL*4        :: time
      INTEGER       :: IFRAME, I
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: values
      LOGICAL, DIMENSION(:), ALLOCATABLE :: node2clip_min, node2clip_max

      CALL SCRIPT_HEADER("slf_clip", "1.0",
     &  "Modifier des resultats superieurs/inferieurs a un seuil")

!     Read files
      inname = READ_ANSWER_STR("Input Selafin file")
      CALL SLF_OPEN(inunit, inname, 'r', resin)
      CALL SLF_READ_HEADER(resin)

      outname = READ_ANSWER_PATH("Output Selafin file", inname, '_clip')
      CALL SLF_OPEN(outunit, outname, 'w', resout)
      CALL COPY_HEADER(resin, resout)

!     Define variables
      CALL PRINT_HEADER(resin)
      pos_ref = STDIN_INT("Reference variable?")

      WRITE(*,*) "Enter variable index(es) to clip (0 to finish)"
      CALL STDIN_ARRAY_INT(resin%nbvar, 0, var2clip)

      clip_min = YES_OR_NO("Clip values BELOW a threshold?", .TRUE.)
      IF(clip_min) THEN
        min_thd = STDIN_REAL("Minimum threshold")
        min_val = STDIN_REAL("Replacement value", min_thd, "threshold")
      ENDIF

      clip_max = YES_OR_NO("Clip values ABOVE a threshold?", .TRUE.)
      IF(clip_max) THEN
        max_thd = STDIN_REAL("Maximum threshold")
        max_val = STDIN_REAL("Replacement value", max_thd, "threshold")
      ENDIF

      CALL SLF_WRITE_HEADER(resout)

!     Read/write values for each record time
      ALLOCATE(values(resout%nnode, resin%nbvar))
      IF(clip_min) ALLOCATE(node2clip_min(resin%nnode))
      IF(clip_max) ALLOCATE(node2clip_max(resin%nnode))
      IFRAME = 0

 80   CALL SLF_READ_TIME(resin, time, eof)
      IF(eof) GOTO 90
      values = SLF_READ_FRAME(resin)

      IF(clip_min) THEN
!       Build clip_min :
!         logical arrays for values below a minimum threshold
        WHERE(values(:,pos_ref) < min_thd)
          node2clip_min = .TRUE.
        ELSEWHERE
          node2clip_min = .FALSE.
        END WHERE

!       Assign min_val for values exceeding the threshold
        DO I = 1, SIZE(var2clip)
          WHERE(node2clip_min)
            values(:,var2clip(I)) = min_val
          END WHERE
        ENDDO
      ENDIF

      IF(clip_max) THEN
!       Build clip_max :
!         logical arrays for values exceeding a maximum threshold
        WHERE(values(:,pos_ref) > max_thd)
          node2clip_max = .TRUE.
        ELSEWHERE
          node2clip_max = .FALSE.
        END WHERE

!       Assign max_val for values exceeding the threshold
        DO I = 1, SIZE(var2clip)
          WHERE(node2clip_max)
            values(:,var2clip(I)) = max_val
          END WHERE
        ENDDO
      ENDIF

!     Write clipped values
      CALL SLF_WRITE_FRAME(resout, time, values)
      IFRAME = IFRAME + 1
      GOTO 80

 90   CALL CLOSE_RES(resin)
      CALL CLOSE_RES(resout)

      END PROGRAM SLF_CLIP
