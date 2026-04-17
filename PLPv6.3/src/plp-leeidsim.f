      SUBROUTINE LeeIndSimDim(ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcIndSim

      TYPE(PAR_DIMS) Dim
      INTEGER ULog
      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead

      INTEGER NSimul

      URead = Abrir(NArcIndSim, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeindsim: Error, no existe archivo ',       &
     &        NArcIndSim, '.'
         WRITE(ULog, '(3A)') 'leeindsim: Error, no existe archivo ',    &
     &        NArcIndSim, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimul
      IF (Dim%Eta .EQ. 1) NSimul = 1

      Dim%Simul = NSimul
      Dim%Apert = Dim%Simul

      CALL Cerrar(URead)

      RETURN
      END

!*****************************************************************
!     Subrutina que lee las simulaciones. En realidad lee los indices qu
!     apuntan a los caudales que estan en el archivo 'NArcAflCen>'.
!*****************************************************************
      SUBROUTINE LeeIndSim(FDatChe, NEtapa,           &
     &     NSimul, SimulInd, NClase,                  &
     &     FInterfaz, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcIndSim

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Lee los caudales por etapa y por simulacion
      CHARACTER*12 AuxVar
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER Abrir
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NDia
      INTEGER NEtaCau
      INTEGER NClase
      INTEGER ISim
      INTEGER NSimul
      INTEGER NumEta
      INTEGER ULog
      INTEGER URead
      INTEGER Ind(Dim%Simul)
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcIndSim, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeindsim: Error, no existe archivo ',       &
     &        NArcIndSim, '.'
         WRITE(ULog, '(3A)') 'leeindsim: Error, no existe archivo ',    &
     &        NArcIndSim, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimul, NEtaCau
      IF (NEtapa .EQ. 1) NSimul = 1
      IF (NSimul .GT. Dim%Simul) THEN
         IF (FDatChe) THEN
            WRITE(6, '(A, I2, A, I2, A)')                               &
     &           'leeindsim: Error, NSimul = ', NSimul,                 &
     &           ' > DimSimul = ', Dim%Simul, '.'
            WRITE(ULog, '(A, I2, A, I2, A)')                            &
     &           'leeindsim: Error, NSimul = ', NSimul,                 &
     &           ' > DimSimul = ', Dim%Simul, '.'
            FStop = .TRUE.
         ENDIF
         NSimul = Dim%Simul
         WRITE(6, '(A, I3)') 'leeindsim:  NSimul := ', Dim%Simul
         WRITE(ULog, '(A, I3)') 'leeindsim: NSimul := ', Dim%Simul
         FWarning = .TRUE.
      ENDIF
!
!     Asignacion por defecto de los indices de las simulaciones
      DO IEta = 1, NEtapa
         DO ISim = 1, NSimul
            SimulInd(ISim, IEta) = 1
         ENDDO
      ENDDO
!
      READ(URead, '(A1)') AuxVar
      DO IEta = 1, NEtaCau
         READ(URead, *) NDia, NumEta, (Ind(ISim), ISim = 1, NSimul)
         IF ((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
            IF (FDatChe) THEN
               WRITE(6, '(2A, I4, A, I4, A)')                           &
     &              'leeindsim: Numero de etapa fuera de ',             &
     &              'rango: 1 < = ', NumEta, ' < = ', NEtapa, '.'
               WRITE(ULog, '(2A, I4, A, I4, A)')                        &
     &              'leeindsim: Numero de etapa fuera de ',             &
     &              'rango: 1 < = ', NumEta, ' < = ', NEtapa, '.'
               FStop = .TRUE.
            ENDIF
         ELSE
            DO ISim = 1, NSimul
               IF (Ind(ISim) .GT. NClase) THEN
                  WRITE(6, '(A, I3, A)')                                &
     &                 'leeidsim: Error, etapa ', NumEta, '.'
                  WRITE(ULog, '(A, I3, A)')                             &
     &                 'leeidsim: Error, etapa ', NumEta, '.'
                  WRITE(6, '(A, I3, A, I3, A, I3, A)')                  &
     &                 'leeidsim: Error, Ind(', ISim, ') = ',           &
     &                 Ind(ISim), ' > NClase = ', NClase, '.'
                  WRITE(ULog, '(A, I3, A, I3, A, I3, A)')               &
     &                 'leeidsim: Error, Ind(', ISim, ') = ',           &
     &                 Ind(ISim), ' > NClase = ', NClase, '.'
                  STOP 1
               ENDIF
               SimulInd(ISim, NumEta) = Ind(ISim)
            ENDDO
         ENDIF
      ENDDO
      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leemat: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leemat: Hay warnings.'
            WRITE(6, '(A)') 'leemat: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END
