      SUBROUTINE LeeManCenSDim(Dim, ULog)

      USE PLP, ONLY : PAR_DIMS, NArcManCenS

      TYPE(PAR_DIMS) Dim
      INTEGER ULog

      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead

      INTEGER NSimMan

      Dim%CenManS = 1

      URead = Abrir(NArcManCenS, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         RETURN
      ENDIF

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimMan
      Dim%CenManS = NSimMan + 1

      CALL Cerrar(URead)

      END

!*********************************
!     Subrutina Mantenimiento Centrales
!*********************************
      SUBROUTINE LeeManCenS(FDatChe, NBloques, NSimul, NCentral,        &
     &     CenNom, CenPMin, CenPMax,                                    &
     &     CenManSInd, FSeparaLP, FInterfaz, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcManCenS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     lee mantenimiento de centrales
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NBloques
      INTEGER NSimul
      INTEGER NCentral
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      LOGICAL FSeparaLP
      INTEGER FInterfaz
      INTEGER ISim
      INTEGER NSimMan
      INTEGER NumSim

      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
!
      CenManSInd(1:NSimul) = 1

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManCenS, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         RETURN
      ENDIF
      IF (.NOT. FSeparaLP) THEN
         FSeparaLP = .TRUE.
         WRITE(6, '(3A)') 'leemancens: Info, existe archivo ',          &
     &        NArcManCenS, ', activando modo FSeparaLP.'
         WRITE(ULog, '(3A)') 'leemancens: Info, existe archivo ',       &
     &        NArcManCenS, ', activando modo FSeparaLP.'
      ENDIF

      DO ISim = 2, Dim%CenManS
         CenPMax(1:NCentral, 1:NBloques, ISim) = CenPMax(1:NCentral, 1:NBloques, 1)
         CenPMin(1:NCentral, 1:NBloques, ISim) = CenPMin(1:NCentral, 1:NBloques, 1)
      ENDDO

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimMan
      DO ISim = 1, NSimMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NumSim
         IF ((NumSim .LT. 1) .OR. (NumSim .GT. NSimul)) THEN
            IF (FDatChe) THEN
               WRITE(6, '(A, 2(A, I4), A)')                             &
     &              'leemancens: Numero de simulacion fuera de rango',  &
     &              '1<', NumSim, '<', NSimul, '.'
               WRITE(ULog, '(A, 2(A, I4), A)')                          &
     &              'leemancens: Numero de simulacion fuera de rango',  &
     &              '1<', NumSim, '<', NSimul, '.'
               FStop = .TRUE.
            ENDIF
         ELSE
            CenManSInd(NumSim) = ISim + 1
            CALL LeeManCenI(FDatChe, NBloques, NCentral,                &
     &           CenNom,                                                &
     &           CenPMin(1, 1, CenManSInd(NumSim)),                   &
     &           CenPMax(1, 1, CenManSInd(NumSim)),                   & 
     &           ULog, URead, FStop, FWarning, Dim)
            IF (FStop) THEN 
               EXIT
            ENDIF
         ENDIF
      ENDDO
      CALL Cerrar(URead)

      IF (FStop) THEN
         STOP 1
      ENDIF

      IF (FWarning) THEN
         WRITE(ULog, '(A)') 'leemances: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leemances: Hay warnings.'
            WRITE(6, '(A)') 'leemances: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END
