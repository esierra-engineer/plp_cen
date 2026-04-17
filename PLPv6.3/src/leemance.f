!*********************************
!     Subrutina Mantenimiento Centrales
!*********************************
      SUBROUTINE LeeManCen(FDatChe, NBloques, NCentral,                 &
     &     CenNom, CenPMin, CenPMax, FInterfaz, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcManCen

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     lee mantenimiento de centrales
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NBloques
      INTEGER NCentral
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
!
      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManCen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leemancen: Error, no existe archivo ',       &
     &        NArcManCen, '.'
         WRITE(ULog, '(3A)') 'leemancen: Error, no existe archivo ',    &
     &        NArcManCen, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      
      CALL LeeManCenI(FDatChe, NBloques, NCentral,                      &
     &     CenNom, CenPMin, CenPMax, ULog,                              & 
     &     URead, FStop, FWarning, Dim)

      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      IF (FWarning) THEN
         WRITE(ULog, '(A)') 'leemance: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leemance: Hay warnings.'
            WRITE(6, '(A)') 'leemance: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END

      SUBROUTINE LeeManCenI(FDatChe, NBloques, NCentral,                &
     &     CenNom, CenPMin, CenPMax, ULog,                              & 
     &     URead, FStop, FWarning, Dim)

      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     lee mantenimiento de centrales
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*48 NomCen
      INTEGER ICen
      INTEGER ICenMan
      INTEGER IBlo
      INTEGER NBloques
      INTEGER NCenMan
      INTEGER NCentral
      INTEGER NDia
      INTEGER NBloMan
      INTEGER NPot
      INTEGER NumCen
      INTEGER NumBlo
      INTEGER NumIPot
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION PotMax
      DOUBLE PRECISION PotMin
!


      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCenMan
      DO ICenMan = 1, NCenMan
         READ(URead, '(A1)') AuxVar
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
            WRITE(ULog, '(3A)') 'leemancen: Warning, central ', NomCen,   &
     &           ' no existe.'
            FWarning = .TRUE.
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NBloMan, NumIPot
         READ(URead, '(A1)') AuxVar
         DO IBlo = 1, NBloMan
            READ(URead, *) NDia, NumBlo, NPot, PotMin, PotMax
            IF ((NumBlo .LT. 1) .OR. (NumBlo .GT. NBloques)) THEN
               IF (FDatChe) THEN
                  WRITE(6, '(3A)')                                   &
     &                 'leemancen: Error en datos central ',         &
     &                 NomCen, '.'
                  WRITE(6, '(2(A, I4), A)')                          &
     &                 'leemancen: Numero de etapa fuera ',          &
     &                 'de rango: 1 <', NumBlo, ' <', NBloques, '.'
                  WRITE(ULog, '(3A)')                                &
     &                 'leemancen: Error en datos central ',         &
     &                 NomCen, '.'
                  WRITE(ULog, '(2(A, I4), A)')                       &
     &                 'leemancen: Numero de etapa fuera ',          &
     &                 'de rango: 1 <', NumBlo, ' <', NBloques, '.'
                  FStop = .TRUE.
               ENDIF
            ELSE
               IF (NumCen .GT. 0) THEN
                  CenPMin(NumCen, NumBlo) = PotMin
                  CenPMax(NumCen, NumBlo) = PotMax
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END
