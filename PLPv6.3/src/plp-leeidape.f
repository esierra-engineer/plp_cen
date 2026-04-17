!*****************************************************************
!     Subrutina que lee las aperturas para cada simulacion. En realidad
!     lee los indices que apuntan a los caudales que estan en el archivo
!     '<NArcAflCen>'.
!*****************************************************************
      SUBROUTINE LeeIndApe(FDatChe, NEtapa,            &
     &     NApert, NSimul, NClase, ApertInd,           &
     &     SimulInd, FInterfaz, ULog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Lee los caudales por etapa y por apertura
      CHARACTER*12 AuxVar
      INTEGER ApertInd(Dim%Apert, Dim%Simul, Dim%Eta)
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER Abrir
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NDia
      INTEGER NClase
      INTEGER NEtaCau
      INTEGER IApe
      INTEGER ISim
      INTEGER NAperturas
      INTEGER NApert(Dim%Simul, Dim%Eta)
      INTEGER NSimul
      INTEGER NSimul2
      INTEGER NumEta
      INTEGER URead
      INTEGER ULog
      LOGICAL FDatChe
      LOGICAL FNoLoHeDicho1
      LOGICAL FNoLoHeDicho2
      LOGICAL FNoLoHeDicho3
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz

      INTEGER Ind(Dim%Apert)

!
      FNoLoHeDicho1 = .TRUE.
      FNoLoHeDicho2 = .TRUE.
      FNoLoHeDicho3 = .TRUE.
      FStop = .FALSE.
      FWarning = .FALSE.
!
!     Inicializacion de los arreglos con las aperturas de las simulacion
!     Por defecto, estos se construyen como si correspondieran a lo que
!     se llama 'hidrologias dependientes.'
      DO IEta = 1, NEtapa
         DO ISim = 1, NSimul
            NApert(ISim, IEta) = 1
            DO IApe = 1, NApert(ISim, IEta)
               ApertInd(IApe, ISim, IEta) = SimulInd(ISim, IEta)
            ENDDO
         ENDDO
      ENDDO
      URead = Abrir(NArcIndApe, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeindape: Error, no existe archivo ',       &
     &        NArcIndApe, '.'
         WRITE(ULog, '(3A)') 'leeindape: Error, no existe archivo ',    &
     &        NArcIndApe, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimul2, NEtaCau
      IF (NSimul2 .GT. NSimul) THEN
         IF (FDatChe) THEN
            WRITE(6, '(A, I2, A, I2, A)')                               &
     &           'leeindape: Error, NSimul2 = ',                        &
     &           NSimul2, ' > ', NSimul, ' = NSimul.'
            WRITE(ULog, '(A, I2, A, I2, A)')                            &
     &           'leeindape: Error, NSimul2 = ',                        &
     &           NSimul2, ' > ', NSimul, ' = NSimul.'
            FStop = .TRUE.
         ENDIF
         NSimul2 = NSimul
         WRITE(6, '(A, I3)') 'leeindape: NSimul2 := ', NSimul
         WRITE(ULog, '(A, I3)') 'leeindape: NSimul2 := ', NSimul
         FWarning = .TRUE.
      ENDIF
      DO ISim = 1, NSimul2
         READ(URead, '(A1)') AuxVar
         DO IEta = 1, NEtaCau
            READ(URead, *) NDia, NumEta, NAperturas,                    &
     &           (Ind(IApe), IApe = 1, NAperturas)
            IF ((NumEta .LT. 2) .OR. (NumEta .GT. NEtapa)) THEN
               IF (FNoLoHeDicho1) THEN
                  IF (FDatChe) THEN
                     WRITE(6, '(2A, I2, A, I2, A)')                     &
     &                    'leeindape: Numero de etapa fuera de ',       &
     &                    'rango: 2 < = ', NumEta, ' < = ', NEtapa, '.'
                     WRITE(ULog, '(2A, I2, A, I2, A)')                  &
     &                    'leeindape: Numero de etapa fuera de ',       &
     &                    'rango: 2 < = ', NumEta, ' < = ', NEtapa, '.'
                     FStop = .TRUE.
                  ENDIF
                  FNoLoHeDicho1 = .FALSE.
               ENDIF
            ELSE
               IF (NAperturas .GT. Dim%Apert) THEN
                  IF (FNoLoHeDicho2) THEN
                     IF (FDatChe) THEN
                        WRITE(6, '(A, I2, A, I2, A)')                   &
     &                       'leeindape: Error, NAperturas = ',         &
     &                       NAperturas,                                &
     &                       ' > DimApert = ', Dim%Apert, '.'
                        WRITE(ULog, '(A, I2, A, I2, A)')                &
     &                       'leeindape: Error, NAperturas = ',         &
     &                       NAperturas,                                &
     &                       ' > DimApert = ', Dim%Apert, '.'
                        FStop = .TRUE.
                     ENDIF
                     WRITE(6, '(A, I3, A)') 'leeindape: NAperturas := ',&
     &                    Dim%Apert, '.'
                     WRITE(ULog, '(A, I3, A)')                          &
     &                    'leeindape: NAperturas := ',                  &
     &                    Dim%Apert, '.'
                     FNoLoHeDicho2 = .FALSE.
                  ENDIF
                  NAperturas = Dim%Apert
                  FWarning = .TRUE.
               ENDIF
               IF (FCDEC .AND. (NAperturas .GT. NClase)) THEN
                  IF (FNoLoHeDicho3) THEN
                     IF (FDatChe) THEN
                        WRITE(6, '(A, I2, A, I2)')                      &
     &                       'leeindape: Error, NAperturas = ',         &
     &                       NAperturas,                                &
     &                       ' > NClase = ', NClase, '.'
                        WRITE(ULog, '(A, I2, A, I2, A)')                &
     &                       'leeindape: Error, NAperturas = ',         &
     &                       NAperturas,                                &
     &                       ' > NClase = ', NClase, '.'
                        FStop = .TRUE.
                     ENDIF
                     WRITE(6, '(A, I3, A)') 'leeindape: NAperturas := ',&
     &                    NClase, '.'
                     WRITE(ULog, '(A, I3, A)')                          &
     &                    'leeindape: NAperturas := ',                  &
     &                    NClase, '.'
                     FNoLoHeDicho3 = .FALSE.
                  ENDIF
                  NAperturas = NClase
                  FWarning = .TRUE.
               ENDIF
               NApert(ISim, NumEta) = NAperturas
               DO IApe = 1, NApert(ISim, NumEta)
                  ApertInd(IApe, ISim, NumEta) = Ind(IApe)
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      CALL Cerrar(URead)

      IF (FStop) THEN
         STOP 1
      ENDIF
      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leeindape: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leeindape: Hay warnings.'
            WRITE(6, '(A)') 'leeindape: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END
