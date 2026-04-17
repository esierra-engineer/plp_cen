      SUBROUTINE LeeManEmbSDim(Dim, ULog)

      USE PLP, ONLY : PAR_DIMS, NArcManEmbS

      TYPE(PAR_DIMS) Dim
      INTEGER ULog

      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead

      INTEGER NSimMan

      Dim%EmbManS = 1

      URead = Abrir(NArcManEmbS, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         RETURN
      ENDIF

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimMan
      Dim%EmbManS = NSimMan + 1

      CALL Cerrar(URead)

      END

!******************************************************************
      SUBROUTINE LeeManEmbS(FDatChe, NEtapa, NSimul, NCenEmb, CenNom,   &
     &     EmbFEsc, EmbVMin, EmbVMax, EmbVMaxA,                         &
     &     EmbManSInd, FSeparaLP, FInterfaz, ULog, Dim)
!******************************************************************

      USE PLP, ONLY : PAR_DIMS, NArcManEmbS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!     lee mantenimientos de embalses
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER ISim
      INTEGER NEtapa
      INTEGER NSimul
      INTEGER NCenEmb
      INTEGER NSimMan
      INTEGER NumSim
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      LOGICAL FWarning
      LOGICAL FSeparaLP
      INTEGER FInterfaz
      DOUBLE PRECISION EmbFEsc(Dim%Emb)

      INTEGER EmbManSInd(Dim%Simul)
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta, Dim%EmbManS)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta, Dim%EmbManS)
      DOUBLE PRECISION EmbVMaxA(Dim%Emb)
      

      INTEGER IVol
!
      EmbManSInd(1:NSimul) = 1
 
      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManEmbS, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         DO IVol = 1, NCenEmb
            EmbVMaxA(IVol) = MAXVAL(EmbVMax(IVol, 1:NEtapa, 1))
         ENDDO
         RETURN
      ENDIF

      IF (.NOT. FSeparaLP) THEN
         FSeparaLP = .TRUE.
         WRITE(6, '(3A)') 'leemanembs: Info, existe archivo ',       &
     &        NArcManEmbS, ', activando modo FSeparaLP.'
         WRITE(ULog, '(3A)') 'leemanembs: Info, existe archivo ',    &
     &        NArcManEmbS, ', activando modo FSeparaLP.'
      ENDIF

      DO ISim = 2, Dim%EmbManS
         EmbVMax(1:NCenEmb, 1:NEtapa, ISim) = EmbVMax(1:NCenEmb, 1:NEtapa, 1)
         EmbVMin(1:NCenEmb, 1:NEtapa, ISim) = EmbVMin(1:NCenEmb, 1:NEtapa, 1)
      ENDDO


      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NSimMan
      DO ISim=1, NSimMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NumSim
         IF ((NumSim .LT. 1) .OR. (NumSim .GT. NSimul)) THEN
            IF (FDatChe) THEN
               WRITE(6, '(A, 2(A, I4), A)')                             &
     &              'leemanembs: Numero de simulacion fuera de rango',  &
     &              '1<', NumSim, '<', NSimul, '.'
               WRITE(ULog, '(A, 2(A, I4), A)')                          &
     &              'leemanembs: Numero de simulacion fuera de rango',  &
     &              '1<', NumSim, '<', NSimul, '.'
               FStop = .TRUE.
            ENDIF
         ELSE
            EmbManSInd(NumSim) = ISim + 1           
            CALL LeeManEmbi(FDatChe, NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &           EmbVMin(1, 1, EmbManSInd(NumSim)),                     &
     &           EmbVMax(1, 1, EmbManSInd(NumSim)),                     &
     &           ULog, URead, FStop, FWarning, Dim)
            IF (FStop) THEN 
               EXIT 
            ENDIF
         ENDIF
      ENDDO
      CALL Cerrar(URead)

      DO IVol = 1, NCenEmb
         EmbVMaxA(IVol) = MAXVAL(EmbVMax(IVol, 1:NEtapa, 1:Dim%EmbManS))
      ENDDO

      IF (FStop) THEN
         STOP 1
      ENDIF
      IF (FWarning) THEN
         WRITE(ULog, '(A)') 'leemanems: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leemanems: Hay warnings.'
            WRITE(6, '(A)') 'leemanems: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF


      RETURN
      END
