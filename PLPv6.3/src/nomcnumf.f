      SUBROUTINE NomCen2NumFil(NArcCenFil, NumFil, FWarning, NomCen,    &
     &     Objeto, ULog)
!
      CHARACTER*24 NArcCenFil
      CHARACTER*48 NomCen
      CHARACTER*42 Objeto
      CHARACTER*48 NomEmb
      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER FiltNCen
      INTEGER IFil
      INTEGER ITra
      INTEGER NTramo
      INTEGER NumFil
      INTEGER ULog
      INTEGER URead
      LOGICAL FWarning
!
      URead = Abrir(NArcCenFil, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecenfil: No existe archivo ',              &
     &        NArcCenFil, '.'
         WRITE(ULog, '(3A)') 'leecenfil: No existe archivo ',           &
     &        NArcCenFil, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FiltNCen
      NumFil = 0
      IFil = 1
      DO WHILE ((NumFil .EQ. 0) .AND. (IFil .LE. FiltNCen))
         READ(URead, '(A1)') AuxVar
         READ(URead, '(A1)') AuxVar
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         IF (NomCen .EQ. NomEmb) THEN
            NumFil = IFil
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, '(A1)') AuxVar
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NTramo
         READ(URead, '(A1)') AuxVar
         DO ITra = 1, NTramo
            READ(URead, '(A1)') AuxVar
         ENDDO
         IFil = IFil + 1
      ENDDO
      CALL Cerrar(URead)
      IF (NumFil .EQ. 0) THEN
         WRITE(6, '(A, $)') 'nomcnumf: Error, '
         WRITE(6, '(A, $)') Objeto
         WRITE(6, '(A, $)') ' no existe.'
         WRITE(6, *)
         WRITE(ULog, '(3A)') 'nomcnumf: Error, ', Objeto,               &
     &        ' no existe.'
         FWarning = .TRUE.
      ENDIF
      RETURN
      END
