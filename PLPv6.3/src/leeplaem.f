      SUBROUTINE LeePlaEmbDim(NumEtaCF, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcPlaEmbI1, NArcPlaEmbI2

      TYPE(PAR_DIMS) Dim

      EXTERNAL Abrir
      INTEGER Abrir      

      INTEGER NumEtaCF
      INTEGER ULog

      INTEGER IFila
      INTEGER IPDNumIte, IEtapa
      INTEGER URead
      CHARACTER*80 AuxVar

      INTEGER PLPIEmb, EmbPla


      URead = Abrir(NArcPlaEmbI1, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeplaemb: Error, no existe archivo ',       &
     &        NArcPlaEmbI1, '.'
         WRITE(ULog, '(3A)') 'leeplaemb: Error, no existe archivo ',    &
     &        NArcPlaEmbI1, '.'
         STOP 1
      ENDIF

      EmbPla = 0
      READ (URead, *)
      DO WHILE(.TRUE.)
         READ (URead, *, END = 1000)   PLPIEmb
         EmbPla = MAX(PLPIEmb, EmbPla)
      ENDDO
 1000 CALL Cerrar(URead)

      Dim%EmbPla = EmbPla
!
!
!
  
      URead = Abrir(NArcPlaEmbI2, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeplaemb: Error, no existe archivo ',       &
     &        NArcPlaEmbI2, '.'
         WRITE(ULog, '(3A)') 'leeplaemb: Error, no existe archivo ',    &
     &        NArcPlaEmbI2, '.'
         STOP 1
      ENDIF

      READ(URead, '(A1)') AuxVar
      IFila = 0

      DO WHILE(.TRUE.)
         READ (URead, *, END = 2000) IPDNumIte, IEtapa 
         IF (IEtapa .EQ. NumEtaCF) THEN
            IFila = IFila + 1
         ENDIF
      ENDDO

 2000 CALL Cerrar(URead)

      Dim%XFila = IFila
      Dim%XCol = Dim%PDLDAcCol

      RETURN
      END

!*****************************
!     Subrutina Lee Planos Embalses
!*****************************
      SUBROUTINE LeePlaEmb(NCenEmbCFUE, NCenEmb,                        &
     &     PlaCFNCol, PlaCFNFila, EmbCFUE,                              &
     &     CenNom, PlaCFRho, PlaCFBeta0, PlaCFIndCol, PlaCFFO,          &
     &     NumEtaCF, FInterfaz, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcPlaEmbI1, NArcPlaEmbI2

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*48 CenNom(Dim%Cen)

      CHARACTER*80 AuxVar
      DOUBLE PRECISION LDPhiPrv
      DOUBLE PRECISION PlaCFBeta0(Dim%XFila)
      DOUBLE PRECISION PlaCFFO
      DOUBLE PRECISION PlaCFRho(Dim%XCol, Dim%XFila)
      INTEGER IEmb
      INTEGER IEtapa
      INTEGER IFila
      INTEGER IPDNumIte
      INTEGER ISimul
      INTEGER NCenEmb
      INTEGER NCenEmbCFUE
      INTEGER NSimul
      INTEGER NumEmb
      INTEGER NumEtaCF
      INTEGER PlaCFIndCol(Dim%XFila)
      INTEGER PlaCFNCol
      INTEGER PlaCFNFila
      INTEGER PLPIEmb
      INTEGER PLPNCenEmb
      INTEGER ULog
      INTEGER URead
      LOGICAL EmbCFUE(Dim%Emb)
      LOGICAL FOverDimXFila
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz

!
      CHARACTER*48 PLPCenNom
      INTEGER PLPEmbInd(Dim%EmbPla)
      CHARACTER*48 PLPEmbNom(Dim%EmbPla)
      DOUBLE PRECISION GradX(Dim%EmbPla)

!********************************************
!     Lee datos de la funcion de costo futuro
!********************************************
      FStop = .FALSE.
      FWarning = .FALSE.
      FOverDimXFila = .FALSE.
      URead = Abrir(NArcPlaEmbI1, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeplaemb: Error, no existe archivo ',       &
     &        NArcPlaEmbI1, '.'
         WRITE(ULog, '(3A)') 'leeplaemb: Error, no existe archivo ',    &
     &        NArcPlaEmbI1, '.'
         STOP 1
      ENDIF
      PLPNCenEmb = 0
      PLPEmbInd = 0
      PLPEmbNom = ''

      READ (URead, *)
      DO WHILE(.TRUE.)
         READ (URead, *, END = 100)                                     &
     &        PLPIEmb, PLPCenNom
         PLPNCenEmb = MAX(PLPIEmb, PLPNCenEmb)
         NumEmb = 0
         IEmb = 0
         DO WHILE (                                                     &
     &        (NumEmb .EQ. 0) .AND.                                     &
     &        (IEmb .LT. NCenEmb)                                       &
     &        )
            IEmb = IEmb + 1
            IF (CenNom(IEmb) .EQ. PLPCenNom) THEN
               NumEmb = IEmb
            ENDIF
         ENDDO
         IF (PLPIEmb .LE. Dim%EmbPla) THEN
            PLPEmbInd(PLPIEmb) = NumEmb
            PLPEmbNom(PLPIEmb) = PLPCenNom
         ELSE
            WRITE(6, '(A, $)') 'leeplaemb: Error, indice '
            WRITE(6, '(I3, $)') PLPIEmb
            WRITE(6, '(A, $)') ' > '
            WRITE(6, '(I3, $)') Dim%EmbPla
            WRITE(6, '(A, $)') '.'
            WRITE(6, *)
            WRITE(ULog, '(A, $)') 'leeplaemb: Error, indice '
            WRITE(ULog, '(I3, $)') PLPIEmb
            WRITE(ULog, '(A, $)') ' > '
            WRITE(ULog, '(I3, $)') Dim%EmbPla
            WRITE(ULog, '(A, $)') '.'
            WRITE(ULog, *)
            FStop = .TRUE.
         ENDIF
      ENDDO
  100 CALL Cerrar(URead)
      IF (FStop) THEN
         WRITE(6, '(A)') 'leeplaemb: Debe reparar la base de datos.'
         WRITE(ULog, '(A)') 'leeplaemb: Debe reparar la base de datos.'
         STOP 1
      ENDIF
      DO PLPIEmb = 1, PLPNCenEmb
         IF (PLPEmbNom(PLPIEmb) .NE. '') THEN
            IF  (PLPEmbInd(PLPIEmb) .EQ. 0) THEN
               WRITE(ULog, '(A, $)')                                    &
     &              'leeplaemb: Warning, embalse PLP '''
               WRITE(ULog, '(A, $)') PLPEmbNom(PLPIEmb)
               WRITE(ULog, '(A, $)') ''''
               WRITE(ULog, *)
               WRITE(ULog, '(A, $)') '           no se encontro '
               WRITE(ULog, '(A, $)') 'en la base de datos '
               WRITE(ULog, '(A, $)') 'de centrales PCP.'
               WRITE(ULog, *)
               FWarning = .TRUE.
            ELSE
               IF  (.NOT. EmbCFUE(PLPEmbInd(PLPIEmb))) THEN
                  WRITE(ULog, '(A, $)')                                 &
     &                 'leeplaemb: Warning, embalse PLP '''
                  WRITE(ULog, '(A, $)') PLPEmbNom(PLPIEmb)
                  WRITE(ULog, '(A, $)') ''''
                  WRITE(ULog, *)
                  WRITE(ULog, '(A, $)') '           tiene datos de '
                  WRITE(ULog, '(A, $)') 'FCF, pero se ignoran a '
                  WRITE(ULog, '(A, $)') 'peticion del usuario. '
                  WRITE(ULog, *)
                  FWarning = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      URead = Abrir(NArcPlaEmbI2, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeplaemb: Error, no existe archivo ',       &
     &        NArcPlaEmbI2, '.'
         WRITE(ULog, '(3A)') 'leeplaemb: Error, no existe archivo ',    &
     &        NArcPlaEmbI2, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      NCenEmbCFUE = NCenEmb
      IFila = 0
      NSimul = 0

      PlaCFRho = 0.0d0
      DO WHILE(.TRUE.)
         READ (URead, *, END = 200)                                     &
     &        IPDNumIte, IEtapa, ISimul,                                &
     &        LDPhiPrv, (GradX(PLPIEmb), PLPIEmb = 1, PLPNCenEmb)
         IF (IEtapa .EQ. NumEtaCF) THEN
            IF (IFila .LT. Dim%XFila) THEN
               IFila = IFila + 1
               PlaCFIndCol(IFila) = ISimul
               NSimul = MAX(NSimul, ISimul)
               PlaCFBeta0(IFila) = -LDPhiPrv
               DO PLPIEmb = 1, PLPNCenEmb
                  IF (PLPEmbInd(PLPIEmb) .GT. 0) THEN                 
                     IF (EmbCFUE(PLPEmbInd(PLPIEmb))) THEN
                        PlaCFRho(PLPEmbInd(PLPIEmb), IFila) =           &
     &                       GradX(PLPIEmb)
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               FOverDimXFila = .TRUE.
            ENDIF
         ENDIF
      ENDDO
      IF (FOverDimXFila) THEN
         WRITE(6, '(A, I4)')                                            &
     &        'leeplaemb: Error, numero de planos > ',                  &
     &        Dim%XFila
         WRITE(ULog, '(A, I4)')                                         &
     &        'leeplaemb: Error, numero de planos > ',                  &
     &        Dim%XFila
         FStop = .TRUE.
      ENDIF
  200 CALL Cerrar(URead)
      PlaCFNCol = NSimul
      PlaCFNFila = IFila
      IF (NSimul .EQ. 0) THEN
         PlaCFFO = 1.0D0
      ELSE
         PlaCFFO = 1.0d0/DBLE(NSimul)
      ENDIF

      IF (FStop) THEN
         STOP 1
      ENDIF
      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leeplaemb: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN         
            WRITE(6, '(A)') 'leeplaemb: Hay warnings.'
            WRITE(6, '(A)') 'leeplaemb: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      RETURN
      END
