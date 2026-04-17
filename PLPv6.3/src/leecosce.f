!*********************************
!     Subrutina Mantenimiento Centrales
!*********************************
      SUBROUTINE LeeCosCen(FDatChe, NEtapa, NCentral,                  &
     &     CenNom, CenCVar, FactTiempo, FInterfaz, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCosCen

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     lee mantenimiento de centrales
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*48 NomCen
      INTEGER ICen
      INTEGER ICenCos
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NCenCos
      INTEGER NCentral
      INTEGER NDia
      INTEGER NEtaCos
      INTEGER NumCen
      INTEGER NumEta
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION CosVar
      DOUBLE PRECISION FactTiempo
!
      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcCosCen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecoscen: Error, no existe archivo ',       &
     &        NArcCosCen, '.'
         WRITE(ULog, '(3A)') 'leecoscen: Error, no existe archivo ',    &
     &        NArcCosCen, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCenCos
      DO ICenCos = 1, NCenCos
         READ(URead, '(A1)', END = 100) AuxVar
         READ(URead, *) NomCen
         NumCen = 0
         ICen = 1
         DO WHILE ((NumCen .EQ. 0) .AND. (ICen .LE. NCentral))
            IF (CenNom(ICen) .EQ. NomCen) THEN
               NumCen = ICen
            ENDIF
            ICen = ICen + 1
         ENDDO
         IF (NumCen .EQ. 0) THEN
            WRITE(6, '(3A)') 'leecoscen: Error, central ', NomCen,      &
     &           ' no existe.'
            WRITE(ULog, '(3A)') 'leecoscen: Error, central ', NomCen,   &
     &           ' no existe.'
            FWarning = .TRUE.
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NEtaCos
         READ(URead, '(A1)') AuxVar
         DO IEta = 1, NEtaCos
            READ(URead, *) NDia, NumEta, CosVar
            IF ((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
               IF (FDatChe) THEN
                  WRITE(6, '(3A)')                                      &
     &                 'leecoscen: Error en datos central ',            &
     &                 NomCen, '.'
                  WRITE(6, '(2(A, I4), A)')                             &
     &                 'leecoscen: Numero de etapa fuera ',             &
     &                 'de rango: 1 <', NumEta, ' <', NEtapa, '.'
                  WRITE(ULog, '(3A)')                                   &
     &                 'leecoscen: Error en datos central ',            &
     &                 NomCen, '.'
                  WRITE(ULog, '(2(A, I4), A)')                          &
     &                 'leecoscen: Numero de etapa fuera ',             &
     &                 'de rango: 1 <', NumEta, ' <', NEtapa, '.'
                  FStop = .TRUE.
               ENDIF
            ELSE
               IF (NumCen .GT. 0) THEN
                  CenCVar(NumCen, NumEta) = CosVar*                     &
     &                 FactTiempo/3.6d0
               ENDIF
            ENDIF
         ENDDO
      ENDDO
  100 CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      IF (FWarning) THEN
         WRITE(ULog, '(A)') 'leecosce: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leecosce: Hay warnings.'
            WRITE(6, '(A)') 'leecosce: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END
