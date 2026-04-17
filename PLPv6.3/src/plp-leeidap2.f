!*********************************************************************
!     Subrutina que lee las aperturas para las centrales que tienen
!     independencia hidrologica durante el deshielo, es decir, que son
!     de regimen pluvial.
!*********************************************************************
      SUBROUTINE LeeIndApe2(FDatChe, NEtapa, &
     &     NApert, ApertInd2,                  &
     &     SimulInd, FInterfaz, ULog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!     Lee los caudales por etapa y por apertura
      CHARACTER*12 AuxVar
      INTEGER ApertInd2(Dim%Apert, Dim%Eta)
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER Abrir
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NDia
      INTEGER NEtaCau
      INTEGER IApe
      INTEGER NAperturas
      INTEGER NApert(Dim%Simul, Dim%Eta)
      INTEGER NumEta
      INTEGER URead
      INTEGER Ind(Dim%Apert)
      INTEGER ULog
      LOGICAL FDatChe
      LOGICAL FNoLoHeDicho1
      LOGICAL FNoLoHeDicho2
      LOGICAL FNoLoHeDicho3
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz
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
!
!     PELIGRO, WILL ROBINSON, PELIGRO!(12)
!     Supone que todas las simulaciones tienen igual numero de aperturas
         DO IApe = 1, NApert(1, IEta)
            ApertInd2(IApe, IEta) = SimulInd(1, IEta)
         ENDDO
      ENDDO
      URead = Abrir(NArcIndAp2, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeindape2: Error, no existe archivo ',      &
     &        NArcIndAp2, '.'
         WRITE(ULog, '(3A)') 'leeindape2: Error, no existe archivo ',   &
     &        NArcIndAp2, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NEtaCau
      READ(URead, '(A1)') AuxVar
      DO IEta = 1, NEtaCau
         READ(URead, *) NDia, NumEta, NAperturas,                       &
     &        (Ind(IApe), IApe = 1, NAperturas)
         IF ((NumEta .LT. 2) .OR. (NumEta .GT. NEtapa)) THEN
            IF (FNoLoHeDicho1) THEN
               IF (FDatChe) THEN
                  WRITE(6, '(2A, I2, A, I2, A)')                        &
     &                 'leeindape2: Numero de etapa fuera de ',         &
     &                 'rango: 2 < = ', NumEta, ' < = ', NEtapa, '.'
                  WRITE(ULog, '(2A, I2, A, I2, A)')                     &
     &                 'leeindape2: Numero de etapa fuera de ',         &
     &                 'rango: 2 < = ', NumEta, ' < = ', NEtapa, '.'
                  FStop = .TRUE.
               ENDIF
               FNoLoHeDicho1 = .FALSE.
            ENDIF
         ELSE
            IF (NAperturas .NE. NApert(1, IEta)) THEN
               IF (FNoLoHeDicho2) THEN
                  IF (FDatChe) THEN
                     WRITE(6, '(A, I2, A, I2, A, I2, A)')               &
     &                    'leeindape2: Error, NAperturas = ',           &
     &                    NAperturas,                                   &
     &                    ' = NApert(1, ', IEta, ') = ',                &
     &                    NApert(1, IEta), '.'
                     WRITE(ULog, '(A, I2, A, I2, A, I2, A)')            &
     &                    'leeindape2: Error, NAperturas = ',           &
     &                    NAperturas,                                   &
     &                    ' = NApert(1, ', IEta, ') = ',                &
     &                    NApert(1, IEta), '.'
                     FStop = .TRUE.
                  ENDIF
                  WRITE(6, '(A, I3, A)') 'leeindape2: NAperturas := ',  &
     &                 NApert(1, IEta), '.'
                  WRITE(ULog, '(A, I3, A)')                             &
     &                 'leeindape2: NAperturas := ',                    &
     &                 NApert(1, IEta), '.'
                  FNoLoHeDicho2 = .FALSE.
               ENDIF
               NAperturas = NApert(1, IEta)
               FWarning = .TRUE.
            ENDIF
            DO IApe = 1, NApert(1, NumEta)
               ApertInd2(IApe, NumEta) = Ind(IApe)
            ENDDO
         ENDIF
      ENDDO
      CALL Cerrar(URead)

      IF (FStop) THEN
         STOP 1
      ENDIF
      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leeindape2: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leeindape2: Hay warnings.'
            WRITE(6, '(A)') 'leeindape2: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END
