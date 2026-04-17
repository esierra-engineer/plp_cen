!********************
!     Subrutina salva
!********************
      SUBROUTINE Salva(ietapa, pdnumite, isimul, iapert,       &
     &     pmnumite, ext, FPLP, iciclo, ULog, lp)
      USE PLP
!
      CHARACTER*(*), INTENT(IN):: ext
      INTEGER, INTENT(IN):: iapert
      INTEGER, INTENT(IN):: ietapa
      INTEGER, INTENT(IN):: isimul
      INTEGER(C_SIZE_T), INTENT(IN):: lp
      INTEGER, INTENT(IN):: pdnumite
      INTEGER, INTENT(IN):: pmnumite
      INTEGER, INTENT(IN):: iciclo
      INTEGER, INTENT(IN):: ULog
      LOGICAL, INTENT(IN):: FPLP

      CALL Salvai(ietapa, pdnumite, isimul, iapert,                     &
     &     pmnumite, ext, FPLP, iciclo, ULog, lp, .FALSE.)
      RETURN
      END
      
      
      SUBROUTINE Salvai(ietapa, pdnumite, isimul, iapert,               &
     &     pmnumite, ext, FPLP, iciclo, ULog, lp, iverb)
      USE OSI

!
      CHARACTER*(*), INTENT(IN):: ext
      INTEGER, INTENT(IN):: iapert
      INTEGER, INTENT(IN):: ietapa
      INTEGER, INTENT(IN):: isimul
      INTEGER(C_SIZE_T), INTENT(IN):: lp
      INTEGER, INTENT(IN):: pdnumite
      INTEGER, INTENT(IN):: pmnumite
      INTEGER, INTENT(IN):: iciclo
      INTEGER, INTENT(IN):: ULog
      LOGICAL, INTENT(IN):: FPLP
      LOGICAL, INTENT(IN):: iverb
!     locals
      CHARACTER*80 filename
      INTEGER lcadena

!
      CALL name4salva(filename, iapert, ietapa, isimul, pdnumite,       &
     &     pmnumite, ext, FPLP, iciclo)
!
!     WRITE to a file.
      CALL osi_lp_writelp (lp, filename, 1.0d-12)
      
      IF (iverb) THEN
         WRITE(ULog, '(A, A)')'Salva archivo ', &
     &        filename(1:lcadena(filename))
      ENDIF

      RETURN
      END
