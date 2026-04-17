      SUBROUTINE PDMsjFin(ZSPFBest, ZSDF, PDNumIte, PDMaxIte,           &
     &     NSimul, FLog, ULog)
      IMPLICIT NONE
      INCLUDE 'machcons.fpp'
      INTEGER NSimul
      INTEGER PDMaxIte
      INTEGER PDNumIte
      INTEGER ULog
      LOGICAL FLog
      DOUBLE PRECISION Error
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION ZSPFBest
      DOUBLE PRECISION epsilon
!
      IF (FLog) THEN
         Error = ZSPFBest - ZSDF
         epsilon = MAX(DABS (ZSPFBest), DABS (ZSDF))*DBLE (SEPSILON)
         IF ((NSimul .EQ. 1) .AND.                                      &
     &        (Error .LT. - epsilon)) THEN
            IF (FLog) THEN
               WRITE(ULog, '(2A, I3)') 'pdmsjfin: Cruce de funciones ', &
     &              'objetivos entre la FD y FP.'
            ENDIF
         ENDIF
         IF (PDNumIte .GE. PDMaxIte) THEN
            IF (FLog) THEN
               WRITE(ULog, '(2A, I3, A)') 'pdmsjfin: Maximo numero de ',&
     &              'iteraciones excedido. PDNumIte = ', PDNumIte, '.'
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END
