!******************************************************************
      SUBROUTINE LeeManEmb(FDatChe, NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &     EmbVMin, EmbVMax, FInterfaz, ULog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!******************************************************************
!     lee mantenimientos de embalses
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
!
      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManEmb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leemanemb: Error, no existe archivo ',       &
     &        NArcManEmb, '.'
         WRITE(ULog, '(3A)') 'leemanemb: Error, no existe archivo ',    &
     &        NArcManEmb, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar

      CALL LeeManEmbi(FDatChe, NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &     EmbVMin, EmbVMax, ULog, URead, FStop, FWarning, Dim)

      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      IF (FWarning) THEN
         WRITE(ULog, '(A)') 'leemanem: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
         WRITE(6, '(A)') 'leemanem: Hay warnings.'
            WRITE(6, '(A)') 'leemanem: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END




!******************************************************************
      SUBROUTINE LeeManEmbi(FDatChe, NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &     EmbVMin, EmbVMax, ULog, URead, FStop, FWarning, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcManEmb

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!******************************************************************
!     lee mantenimientos de embalses
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*48 NomEmb
      INTEGER IEmb
      INTEGER IEmbMan
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NDia
      INTEGER NEmbMan
      INTEGER NEtaMan
      INTEGER NumEmb
      INTEGER NumEta
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION EmbCMinEsc
      DOUBLE PRECISION EmbCMaxEsc
!
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NEmbMan
      DO IEmbMan = 1, NEmbMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         NumEmb = 0
         IEmb = 1
         DO WHILE ((NumEmb .EQ. 0) .AND. (IEmb .LE. NCenEmb))
            IF (CenNom(IEmb) .EQ. NomEmb) THEN
               NumEmb = IEmb
            ENDIF
            IEmb = IEmb + 1
         ENDDO
         IF (NumEmb .EQ. 0) THEN
            WRITE(6, '(3A)') 'leemanemb: Error, embalse ', NomEmb,      &
     &           ' no existe.'
            WRITE(ULog, '(3A)') 'leemanemb: Error, embalse ', NomEmb,   &
     &           ' no existe.'
            FWarning = .TRUE.
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NEtaMan
         READ(URead, '(A1)') AuxVar
         DO IEta = 1, NEtaMan
            READ(URead, *) NDia, NumEta, EmbCMinEsc, EmbCMaxEsc
            IF ((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
               IF (FDatChe) THEN
                  WRITE(6, '(3A)')                                      &
     &                 'leemanemb: Error en datos embalse ', NomEmb, '.'
                  WRITE(6, '(A, 2(A, I4), A)')                          &
     &                 'leemanemb: Numero de etapa fuera de rango',     &
     &                 '1<', NumEta, '<', NEtapa, '.'
                  WRITE(ULog, '(3A)')                                   &
     &                 'leemanemb: Error en datos embalse ', NomEmb, '.'
                  WRITE(ULog, '(A, 2(A, I4), A)')                       &
     &                 'leemanemb: Numero de etapa fuera de rango',     &
     &                 '1<', NumEta, '<', NEtapa, '.'
                  FStop = .TRUE.
               ENDIF
            ELSE
               IF (NumEmb .GT. 0) THEN
                  IF (EmbCMinEsc .GT. EmbCMaxEsc) THEN
                     WRITE(6, '(2A)')                                   &
     &                    'leecnfcen: Error en los datos central ',     &
     &                    CenNom(NumEmb)
                     WRITE(6, '(2A)') 'leecnfcen: Las cotas minima ',   &
     &                    'y maxima de mantenimiento estan mal ',       &
     &                    'definidas.'
                     WRITE(ULog, '(2A)')                                &
     &                    'leecnfcen: Error en los datos central ',     &
     &                    CenNom(NumEmb)
                     WRITE(ULog, '(2A)') 'leecnfcen: Las cotas minima ',&
     &                    'y maxima de mantenimiento estan mal ',       &
     &                    'definidas.'
                     FStop = .TRUE.
                  ELSE
                     EmbVMin(NumEmb, NumEta) =                          &
     &                    EmbCMinEsc*EmbFEsc(NumEmb)/1D3
                     EmbVMax(NumEmb, NumEta) =                          &
     &                    EmbCMaxEsc*EmbFEsc(NumEmb)/1D3
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
