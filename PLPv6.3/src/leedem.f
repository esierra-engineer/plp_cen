!********************
!>    Subrutina LeeDemanda
!>    lee las demandas en las barras
!********************
      SUBROUTINE LeeDem(FDatChe, NBloque, NBarra, BloPot, BarNom,       &
     &     ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcDem

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     comun a todas las rutinas:
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*48 NomBar
      INTEGER IBar
      INTEGER IBarDem
      INTEGER IBlo
      INTEGER NBarDem
      INTEGER NBarra
      INTEGER NBloque
      INTEGER NDia
      INTEGER NBloDem
      INTEGER NumBar
      INTEGER NumBlo
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION PotDem
!     codigo:
!************************
!     Lee datos pcpdem.dat
!************************
      FStop = .FALSE.
      URead = Abrir(NArcDem, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leedem: Error, no existe archivo ',          &
     &        NArcDem, '.'
         WRITE(ULog, '(3A)') 'leedem: Error, no existe archivo ',       &
     &        NArcDem, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NBarDem
!
!     Inicializa demandas en cero
!***************************
      BloPot = 0.0d0

!
!     Lee Demandas
!************
      DO IBarDem = 1, NBarDem
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomBar
         NumBar = 0
         IBar = 1
         DO WHILE ((NumBar .EQ. 0) .AND. (IBar .LE. NBarra))
            IF (BarNom(IBar) .EQ. NomBar) THEN
               NumBar = IBar
            ENDIF
            IBar = IBar + 1
         ENDDO
         IF (NumBar .EQ. 0) THEN
            IF (FDatChe) THEN
               WRITE(6, '(3A)') 'leedem: Error, barra ', NomBar,        &
     &              ' no existe.'
               WRITE(ULog, '(3A)') 'leedem: Error, barra ', NomBar,     &
     &              ' no existe.'
               FStop = .TRUE.
            ENDIF
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NBloDem
         IF (NBloDem .GT. 0) THEN
            READ(URead, '(A1)') AuxVar
            DO IBlo = 1, NBloDem
               READ(URead, *) NDia, NumBlo, PotDem
               IF ((NumBlo .LT. 1) .OR. (NumBlo .GT. NBloque)) THEN
                  IF (FDatChe) THEN
                     WRITE(6, '(3A)')                                   &
     &                    'leedem: Error en datos demanda ',            &
     &                    NomBar, '.'
                     WRITE(6, '(A, 2(A, I4), A)')                       &
     &                    'leedem: Numero de bloque fuera de ',          &
     &                    'rango: 1<', NumBlo, '<', NBloque, '.'
                     WRITE(ULog, '(3A)')                                &
     &                    'leedem: Error en datos demanda ',            &
     &                    NomBar, '.'
                     WRITE(ULog, '(A, 2(A, I4), A)')                    &
     &                    'leedem: Numero de bloque fuera de ',          &
     &                    'rango: 1<', NumBlo, '<', NBloque, '.'
                     FStop = .TRUE.
                  ENDIF
               ELSE
                  IF (NumBar .GT. 0) THEN
                     BloPot(NumBar, NumBlo) =                           &
     &                    BloPot(NumBar, NumBlo) + PotDem
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      RETURN
      END
