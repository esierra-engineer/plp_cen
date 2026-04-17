      SUBROUTINE LeeExtMau(FDatChe, MauleIPar, MauleCPar,      &
     &     NEtapa, NClase, Ulog,  Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_MAULEC, NArcRiego, NArcENDESA, NArcENDESACI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      TYPE(PAR_MAULEC) MauleCPar

!     Convenio del Maule.
      INCLUDE 'maule.fpp'
!
      EXTERNAL Abrir
      INTEGER Abrir
!
      CHARACTER*12 AuxVar
      CHARACTER*24 NArc
      DOUBLE PRECISION CauEta(Dim%Clase)
      INTEGER IClase
      INTEGER IEta
      INTEGER IIPLPExt
      INTEGER IJob
      INTEGER ILeeExtMau
      INTEGER MauleIPar(DimIMaule)
      INTEGER NEtapa
      INTEGER NClase
      INTEGER NDia
      INTEGER NEtaCau
      INTEGER NumEta
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      DO IJob = 1, 3
         IF (IJob .EQ. 1) THEN
!     Extracciones de Regantes.
            ILeeExtMau = ILeeExtMauRie
            IIPLPExt = IIPLPExtRie
            NArc = NArcRiego
         ELSE IF (IJob .EQ. 2) THEN
!     Extracciones de ENDESA Colchon Superior y Res Ext.
            ILeeExtMau = ILeeExtMauEND
            IIPLPExt = IIPLPExtEND
            NArc = NArcENDESA
         ELSE
!     Extracciones de ENDESA Colchon Inferior,
            ILeeExtMau = ILeeExtMauENDCI
            IIPLPExt = IIPLPExtENDCI
            NArc = NArcENDESACI
         ENDIF
         DO IEta = 1, NEtapa
            DO IClase = 1, NClase
               MauleCPar%ExtPar(ILeeExtMau, IClase, IEta) = 0.0d0
            ENDDO
         ENDDO
         IF (MauleIPar(IIPLPExt) .EQ. 0) THEN
            FStop = .FALSE.
            URead = Abrir(NArc, 'OLD', 'SEQUENTIAL', ULog)
            IF (URead .EQ. 0) THEN
               WRITE(6, '(3A)') 'leeextma: Error, no existe archivo ',  &
     &              NArc, '.'
               WRITE(ULog, '(3A)')                                      &
     &              'leeextma: Error, no existe archivo ',              &
     &              NArc, '.'
               STOP 1
            ENDIF
            READ(URead, '(A1)') AuxVar
            READ(URead, *) NEtaCau
            READ(URead, '(A1)') AuxVar
            DO IEta = 1, NEtaCau
!
!     Lectura de caudales aleatorios.
               READ(URead, *) NDia, NumEta, (CauEta(IClase), IClase = 1,&
     &              NClase)
               IF ((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
                  IF (FDatChe) THEN
                     WRITE(6, '(2A, I4, A, I4, A)')                     &
     &                    'leeextma: Error, numero de ',                &
     &                    'etapa fuera de rango: 1 < = ', NumEta,       &
     &                    ' < = ', NEtapa, '.'
                     WRITE(ULog, '(2A, I4, A, I4, A)')                  &
     &                    'leeaextma: Error, numero de ',               &
     &                    'etapa fuera de rango: 1 < = ', NumEta,       &
     &                    ' < = ', NEtapa, '.'
                     FStop = .TRUE.
                  ENDIF
               ELSE
                  DO  IClase = 1, NClase
                     MauleCPar%ExtPar(ILeeExtMau, IClase, NumEta) =     &
     &                    CauEta(IClase)
                  ENDDO
               ENDIF
            ENDDO
            CALL Cerrar(URead)
            IF (FStop) THEN
               STOP 1
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
