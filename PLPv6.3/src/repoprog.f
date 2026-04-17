      SUBROUTINE ReporteProgreso(                                       &
     &     MensajeProgreso, IDeltaProgreso, NProgreso,                  &
     &     InicioReporte, FinReporte)
      IMPLICIT NONE
      CHARACTER*(*) MensajeProgreso
      INTEGER IDeltaProgreso
      INTEGER IIProgreso1
      INTEGER IIProgreso2
      INTEGER IProgreso
      INTEGER NProgreso
      INTEGER NNProgreso
      LOGICAL InicioReporte
      LOGICAL FinReporte
      SAVE IIProgreso1
      SAVE IProgreso
      SAVE NNProgreso
!
      IF (InicioReporte) THEN
         WRITE(6, '(A, ''  0%'', $)') MensajeProgreso
         CALL Vomit(6)
         IIProgreso1 = 0
         IProgreso = 0
         NNProgreso = NProgreso
      ELSEIF (FinReporte) THEN
         CALL WriteCharN(4, CHAR(8))
         WRITE(6, '(''100%'')')
         CALL Vomit(6)
      ELSE
         IProgreso = IProgreso + IDeltaProgreso
         IIProgreso2 =                                                  &
     &        INT(ANINT(100.0*REAL(IProgreso)/REAL(NNProgreso)))
         IF (IIProgreso2 .GT. IIProgreso1) THEN
            CALL WriteCharN(4, CHAR(8))
            WRITE(6, '(I3, ''%'', $)') IIProgreso2
            CALL Vomit(6)
            IIProgreso1 = IIProgreso2
         ENDIF
      ENDIF
      RETURN
      END
