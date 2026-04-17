      SUBROUTINE LeeEtaDim(ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcEta

      INTEGER ULog
      TYPE(PAR_DIMS) Dim


      INTEGER NEtapa, IEta
      INTEGER Year

      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      CHARACTER*12 AuxVar

      URead = Abrir(NArcEta, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeeta: Error, no existe archivo ',          &
     &        NArcEta, '.'
         WRITE(ULog, '(3A)') 'leeeta: Error, no existe archivo ',       &
     &        NArcEta, '.'
         STOP 1
      ENDIF

!     Numero de Etapas 
      READ(URead, '(A12)') AuxVar
      READ(URead, '(A12)') AuxVar
      READ(URead, *) NEtapa
      
      Dim%Eta = NEtapa

      READ(URead, '(A12)') AuxVar
      DO IEta = 1, NEtapa
         READ(URead, *) Year
      ENDDO

      Dim%Year = Year
      
      CALL Cerrar(URead)

      RETURN
      END
      

!************************************
!     Subrutina Lee Duracion de las Etapas
!************************************
      SUBROUTINE LeeEta(NEtapa, Year, Mes, EtaDur, FactTiempo,          &
     &     TipoEtapa, NUnTiPag, FPhi, FDepHidEta, FInterfaz, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcEta, FactTiempoH

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     comun a todas las rutinas:
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER UnidTiempo
      CHARACTER*12 AuxVar
      INTEGER DurEta
      INTEGER IEta
      INTEGER FInterfaz
      INTEGER NEtapa
      INTEGER NHoraTot
      INTEGER NumEta
      INTEGER NUnTiPag
      INTEGER ULog
      INTEGER URead
      LOGICAL FWarning
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION Tasa
      
      CHARACTER*12 TipoEtapa(Dim%Eta)
      LOGICAL FDepHidEta(Dim%Eta)
      INTEGER Mes(Dim%Eta)
      INTEGER Year(Dim%Eta)
      DOUBLE PRECISION EtaDur(Dim%Eta)
      DOUBLE PRECISION FPhi(Dim%Eta)

!     codigo:
!***********************
!     Lee datos pcpeta.dat
!***********************
      FWarning = .False.
      URead = Abrir(NArcEta, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeeta: Error, no existe archivo ',          &
     &        NArcEta, '.'
         WRITE(ULog, '(3A)') 'leeeta: Error, no existe archivo ',       &
     &        NArcEta, '.'
         STOP 1
      ENDIF
!     Numero de Etapas y unidad de tiempo
      READ(URead, '(A12)') AuxVar
      READ(URead, '(A12)') AuxVar
      READ(URead, *) NEtapa, UnidTiempo
      
      IF (UnidTiempo .EQ. 'H') THEN
         FactTiempo = FactTiempoH
         NUnTiPag = 24
      ELSE IF (UnidTiempo .EQ. 'D') THEN
         FactTiempo = 24.0d0*FactTiempoH
         NUnTiPag = 7
      ELSE IF (UnidTiempo .EQ. 'S') THEN
         FactTiempo = 7.0d0*24.0d0*FactTiempoH
         NUnTiPag = 4
      ELSE IF (UnidTiempo .EQ. 'M') THEN
         FactTiempo = ((365*24)/12)*FactTiempoH
         NUnTiPag = 12
      ELSE
         WRITE(6, '(3A)') 'leeeta: Error, la unidad de tiempo = ',      &
     &        UnidTiempo, ' es incorrecta (H/D/S/M).'
         WRITE(ULog, '(3A)') 'leeeta: Error, la unidad de tiempo = ',   &
     &        UnidTiempo, ' es incorrecta (H/D/S/M).'
         STOP 1
      ENDIF

!********************
      NHoraTot = 0
!     Lee Duracion Etapas
!*******************
      READ(URead, '(A12)') AuxVar
      DO IEta = 1, NEtapa
         READ(URead, *) Year(IEta), Mes(IEta), NumEta, FDepHidEta(IEta),&
     &        DurEta, Tasa, TipoEtapa(IEta)
         IF((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
            WRITE(6, '(A)') 'leeeta: Error en datos etapas.'
            WRITE(6, '(A, 2(A, I4), A)')                                &
     &           'leeeta: Numero de etapa fuera de rango: ',            &
     &           '1 <', NumEta, ' <', NEtapa, '.'
            WRITE(ULog, '(A)') 'leeeta: Error en datos etapas.'
            WRITE(ULog, '(A, 2(A, I4), A)')                             &
     &           'leeeta: Numero de etapa fuera de rango: ',            &
     &           '1 <', NumEta, ' <', NEtapa, '.'
            STOP 1
         ENDIF
         EtaDur(IEta) = dble(DurEta)
         FPhi(IEta) = Tasa
         NHoraTot = NHoraTot + DurEta
      ENDDO

      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leeeta: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leeeta: Hay warnings.'
            WRITE(6, '(A)') 'leeeta: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      CALL Cerrar(URead)
      RETURN
      END

