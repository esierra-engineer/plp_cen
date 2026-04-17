!*****************************************
!     Programa de Optimizacion Estocastica
!*****************************************
      PROGRAM PLPMAIN
#ifdef _OPENMP
      USE OMP_LIB
#endif


!
!     Archivos comunes a todas las rutinas.
      USE PLP
      USE OSI

      INCLUDE 'machcons.fpp'
!
      EXTERNAL AbrirDirecto
      INTEGER AbrirDirecto
      EXTERNAL AbrirScratch
      INTEGER AbrirScratch
      EXTERNAL Abrir
      INTEGER Abrir
!
      TYPE(PAR_DIMS) Dim
!
!      CHARACTER*49 MensajeProgreso
      CHARACTER*1, ALLOCATABLE :: CenTipo(:)
      CHARACTER*1 FPerdLin
      CHARACTER*48, ALLOCATABLE :: BarNom(:)
      CHARACTER*48, ALLOCATABLE :: CenNom(:)
      CHARACTER*48, ALLOCATABLE :: LinNom(:)
      CHARACTER*12, ALLOCATABLE :: TipoEtapa(:)
      DOUBLE PRECISION UmbGradX
      DOUBLE PRECISION UmbZSPF
      DOUBLE PRECISION, ALLOCATABLE :: ZSPFBestArr(:)
      DOUBLE PRECISION kappa

      INTEGER ABLMax
      INTEGER, ALLOCATABLE :: AIncid(:, :)
      INTEGER, ALLOCATABLE :: ApertInd(:, :, :)
      INTEGER, ALLOCATABLE :: ApertInd2(:, :)
      INTEGER, ALLOCATABLE :: CenGBar(:)
      INTEGER, ALLOCATABLE :: CenGHid(:, :)
      INTEGER, ALLOCATABLE :: CenInd(:)
      INTEGER, ALLOCATABLE :: CenVHid(:, :)
      LOGICAL FConvPGradX
      LOGICAL FConvPVar
      LOGICAL, ALLOCATABLE :: FDepHidEta(:)
      INTEGER(C_SIZE_T), ALLOCATABLE :: envs(:)
      INTEGER nthreads
      INTEGER i
      INTEGER idual_pmode
      INTEGER iprim_pmode
      INTEGER, ALLOCATABLE :: EstocColInd(:,:,:)
      INTEGER, ALLOCATABLE :: EstocFilaInd(:,:,:)
      INTEGER EstocNCol
      INTEGER EstocNFila
      INTEGER, ALLOCATABLE :: FiltCenInd(:)
      INTEGER, ALLOCATABLE :: FiltColInd(:,:,:)
      INTEGER, ALLOCATABLE :: FiltEmbInd(:)
      INTEGER FiltNCen
      INTEGER, ALLOCATABLE :: FiltNTramo(:)
!     filtrendbeg
      INTEGER, ALLOCATABLE :: RendCenInd(:)
      INTEGER, ALLOCATABLE :: RendEmbInd(:)
      INTEGER, ALLOCATABLE :: RendColInd(:,:,:)
      INTEGER, ALLOCATABLE :: RendFilaInd(:,:,:)
      INTEGER, ALLOCATABLE :: RendNTramo(:)
      INTEGER RendNCen
      INTEGER, ALLOCATABLE :: RendVolColInd(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: RendParam(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: RendProm(:,:)
!     filtrend
!     filtpmaxbeg
      INTEGER, ALLOCATABLE :: PmaxCenInd(:)
      INTEGER, ALLOCATABLE :: PmaxEmbInd(:)
      INTEGER, ALLOCATABLE :: PmaxColInd(:,:,:)
      INTEGER, ALLOCATABLE :: PmaxNTramo(:)
      INTEGER PmaxNCen
      INTEGER, ALLOCATABLE :: PmaxVolColInd(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: PmaxParam(:,:,:)
!     filtpmax
      INTEGER HorPro
      INTEGER IndEta1Imp
      INTEGER IndEta2Imp
      INTEGER IndIteImp
      INTEGER IndSimImp
      INTEGER ISimul
      INTEGER IVol
      INTEGER, ALLOCATABLE :: Kit(:, :)
      INTEGER, ALLOCATABLE :: LinNBar(:, :)
      INTEGER, ALLOCATABLE :: LinNFlu(:)
      INTEGER(C_SIZE_T), ALLOCATABLE :: lp(:, :)
      INTEGER LRec
      INTEGER, ALLOCATABLE :: Mes(:)
      INTEGER MinPro
      INTEGER, ALLOCATABLE ::  NApert(:, :)
      INTEGER NBarra
      INTEGER NEtapa

      INTEGER NBloques
      INTEGER, ALLOCATABLE :: NBloque(:)
      INTEGER, ALLOCATABLE :: BloInd(:, :)
      INTEGER, ALLOCATABLE :: BloEta(:)
      DOUBLE PRECISION, ALLOCATABLE :: BloDur(:)

      INTEGER NCenEmb
      INTEGER NCenEmbCFUE
      INTEGER NCenFalla
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NClase
      INTEGER NFlujo
      INTEGER NLinea
!      INTEGER NProgreso
!      INTEGER NProgreso0
      INTEGER NSimul
      INTEGER PlaCFNCol
      INTEGER PlaCFNFila
      INTEGER NumEtaCF
      INTEGER, ALLOCATABLE :: PlaCFIndCol(:)
      INTEGER NUnTiPag
      INTEGER NVarPhi
      INTEGER, ALLOCATABLE :: PDLDAcColInd(:,:)
      INTEGER, ALLOCATABLE :: PDLDAcFilaInd(:,:)
      INTEGER PDLDAcNCol
      INTEGER PDLDAcNFila
      INTEGER PDMaxIte
      INTEGER, ALLOCATABLE :: PDNCol(:)
      INTEGER, ALLOCATABLE :: PDNFila(:)
      INTEGER, ALLOCATABLE :: PXNCol(:)
      INTEGER, ALLOCATABLE :: PXNFila(:)
      INTEGER SegPro
      INTEGER, ALLOCATABLE :: SimulInd(:, :)
      INTEGER TmpFin(DimTmp)
      INTEGER TmpIni(DimTmp)
      INTEGER ULog
      INTEGER ULogCF
      INTEGER UPreSuf
      INTEGER USimDual0
      INTEGER USimDual1
      INTEGER USimPrimal0
      INTEGER USimPrimal1
      INTEGER, ALLOCATABLE :: Year(:)
      LOGICAL, ALLOCATABLE :: EstocFIndep(:)
      LOGICAL DxAEQy
      LOGICAL, ALLOCATABLE :: EmbCFUE(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbQeLow(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbQeUpp(:)

      LOGICAL FBest
      LOGICAL FZSPFBest
      LOGICAL FOnePhi
      LOGICAL FAfluFict
      LOGICAL FSeparaLP
      INTEGER FlagFilt
      LOGICAL FRendProm
      LOGICAL FRendBdrs
      LOGICAL FSeparaCF
      LOGICAL FAngZero
      LOGICAL FVertReb
      INTEGER FConvLaja
      INTEGER FConvMaule
      LOGICAL FConvNuevos
      LOGICAL FConvAntiguos
      LOGICAL FRestRalco
      LOGICAL FRestGnl
      LOGICAL FScaleQs
      LOGICAL FWarningFalla
      LOGICAL FRestReserva
      LOGICAL FBaterias
      INTEGER FMemMode

      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION, ALLOCATABLE :: ScaleVol(:)
      DOUBLE PRECISION ScaleVolG
      LOGICAL FScaleVdi
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleAng
      LOGICAL FDepHid
      INTEGER FactDBL
      INTEGER FInterfaz
      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      LOGICAL FSeparaCFA, FOneFeasRay
      LOGICAL FInfactSol
      INTEGER FactMXC
      INTEGER DualFactIter


      LOGICAL FFiltVar
      INTEGER, ALLOCATABLE :: FiltVarColInd(:,:)
      INTEGER, ALLOCATABLE :: FiltVarFilInd(:,:)
      INTEGER, ALLOCATABLE :: CenFHid(:)

      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER, ALLOCATABLE :: CenXHid(:)
      INTEGER, ALLOCATABLE :: ExtrCenInd(:)
      DOUBLE PRECISION, ALLOCATABLE :: ExtrMax(:)
      INTEGER, ALLOCATABLE :: ExtrColInd(:, :, :)


      LOGICAL FDatChe
      LOGICAL FGrabaCSV
      LOGICAL FGrabaRES
      LOGICAL FLog
      LOGICAL FPerdTram
      LOGICAL FPreProc
      LOGICAL FPrevia
      LOGICAL FProgEnt
      LOGICAL FSeparaFCF
      LOGICAL FVolFinEmb
      LOGICAL, ALLOCATABLE :: LinFOpe(:, :, :)
      LOGICAL, ALLOCATABLE :: LinFPer(:)
      LOGICAL, ALLOCATABLE :: LinHVDC(:)
      LOGICAL PDSvFl
      LOGICAL ErSvFl
      LOGICAL PsFzFl
      LOGICAL PMSvFl
      LOGICAL FSvLaPs
      LOGICAL PriProgDin

      INTEGER, ALLOCATABLE :: EmbManSInd(:)
      INTEGER, ALLOCATABLE :: CenManSInd(:)
      INTEGER, ALLOCATABLE :: LinManSInd(:)

      DOUBLE PRECISION ABEpsilon
      DOUBLE PRECISION, ALLOCATABLE :: EtaDur(:)
      DOUBLE PRECISION, ALLOCATABLE :: BloPot(:, :)
      DOUBLE PRECISION CCaudFalla
      DOUBLE PRECISION, ALLOCATABLE :: CenCVar(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: CenPMax(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: CenPMin(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: CenVMax(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: CenVMin(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: CenRen(:)
      DOUBLE PRECISION CTrasmision
      DOUBLE PRECISION CVertimiento
      DOUBLE PRECISION, ALLOCATABLE :: EmbVFin(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbVIni(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbVMax(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: EmbVMin(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: EmbVMaxA(:)

      INTEGER NEmbVMinH
      INTEGER, ALLOCATABLE ::  EmbVMinHInd(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbVMinH(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: EmbCMinH(:, :)

      INTEGER, ALLOCATABLE :: EmbVRebInd(:)
      INTEGER NEmbVReb
      INTEGER, ALLOCATABLE :: EmbVRebFilInd(:, :)

      DOUBLE PRECISION, ALLOCATABLE :: EmbVReb(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbCReb(:)
      DOUBLE PRECISION, ALLOCATABLE :: EmbFEsc(:)
      DOUBLE PRECISION, ALLOCATABLE :: PlaCFRho(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: PlaCFBeta0(:)
      DOUBLE PRECISION PlaCFFO
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION, ALLOCATABLE :: FiltParam(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: FiltProm(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: FPhi(:)
      DOUBLE PRECISION, ALLOCATABLE :: HidSPPQAfl(:, :, :)
      DOUBLE PRECISION Lambda
      DOUBLE PRECISION, ALLOCATABLE :: LinRes(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: LinTMax(:, :, :, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: LinTMaxS(:, :, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: LinVNom(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: LinXImp(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: PasQAfl(:, :, :)
      DOUBLE PRECISION PDError
      DOUBLE PRECISION PMError
      DOUBLE PRECISION SFVFOBest
      DOUBLE PRECISION ThetaRef
      DOUBLE PRECISION UmbIntConf
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION ZSPFPromBest
      DOUBLE PRECISION, ALLOCATABLE :: ZSPFArr(:)
!     idesarrollo

      TYPE(PAR_LAJAM) ParLajaM
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_MAULE) ParMaule
      TYPE(PAR_RALCO) ParRalco
      TYPE(PAR_GNL) ParGnl
      TYPE(PAR_GN) ParGn

      TYPE(PAR_RESERVA) ParRes
      TYPE(PAR_BATERIAS) ParBaterias
      LOGICAL FOldMaule
      TYPE(PAR_MAULEC) MauleCPar
      LOGICAL FOldLaja
      TYPE(PAR_LAJAC) LajaCPar

      INTEGER LajaIPar(DimILaja)
      LOGICAL LajaLPar(DimLLaja)
      LOGICAL, ALLOCATABLE :: MesBal(:)
      DOUBLE PRECISION, ALLOCATABLE :: Cau2Vol(:)
      DOUBLE PRECISION, ALLOCATABLE :: CauDemNuReEta(:)
      DOUBLE PRECISION, ALLOCATABLE :: CauDemRegAbaEta(:)
      DOUBLE PRECISION, ALLOCATABLE :: CauDemRegTucaEta(:)
      DOUBLE PRECISION, ALLOCATABLE :: LajaRPar(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: CauRes105Eta(:)
      DOUBLE PRECISION, ALLOCATABLE :: CauConMauEta(:)
      DOUBLE PRECISION, ALLOCATABLE :: MauleRPar(:, :, :)
      INTEGER MauleIPar(DimIMaule)
      LOGICAL, ALLOCATABLE :: MauleLPar(:, :, :)


      CHARACTER*24 PlaneFile
      INTEGER PlaneIterBeg, PlaneIterEnd, PlaneOpenMode, PlaneMaxIter
      DOUBLE PRECISION epopt
      DOUBLE PRECISION eprhs

      INTEGER ScaleOffset, Idx
      INTEGER NCenRes, NZonRes
!
!
      EXTERNAL DxAEQy

      INTEGER shalligo
      INTEGER plpcheckit
      EXTERNAL plpcheckit

      ULog = Abrir (NArcLog, 'UNKNOWN', 'SEQUENTIAL', 6)
      ULogCF = Abrir (NArcLogCF, 'UNKNOWN', 'SEQUENTIAL', ULog)

!     codigo chequeo MAC
!***********
      ! shalligo = plpcheckit()
      shalligo = 0
      IF (shalligo .NE. 0) THEN
         WRITE(6, '(A)')                                                &
     &        'plp: error fatal: Se aborta la corrida.'
         WRITE(ULog, '(A)')                                             &
     &        'plp: error fatal: Se aborta la corrida.'
         STOP
      ENDIF

!     codigo
!***********

      FInterfaz = 1
      CALL InitSolver (Finterfaz, ULog)

!     lee tiempo
      CALL LeeTmp(TmpIni)
!
!     inicializa variables de ambiente

      CALL GET_OPTS( &
     &     eprhs, &
     &     epopt, &
     &     ScaleObj, &
     &     ScalePhi, &
     &     ScaleAng, &
     &     ScaleVolG, &
     &     FScaleQs, &
     &     FScaleVdi, &
     &     FAngZero, &
     &     FConvLaja, &
     &     FConvMaule, &
     &     FRestRalco, &
     &     FRestGnl, &
     &     FRestReserva, &
     &     FBaterias, &
     &     FMemMode, &
     &     FVertReb, &
     &     FSeparaLP, &
     &     FSeparaCF, &
     &     FZSPFBest, &
     &     FOnePhi, &
     &     FInterfaz, &
     &     FDepHid, &
     &     FlagFilt, &
     &     FRendProm, &
     &     FRendBdrs, &
     &     FAfluFict, &
     &     FactEPS, &
     &     FactMLD, &
     &     OptiEPS, &
     &     OptiMLD, &
     &     FactMXC, &
     &     FactDBL, &
     &     FSeparaCFA, &
     &     FOneFeasRay, &
     &     DualFactIter, &
     &     FInfactSol, &
     &     idual_pmode, &
     &     iprim_pmode, &
     &     nthreads, &
     &     Dim)

!
!     Lectura de datos
!**************************************
      CALL LeePath

!
!     Lee parametros debug.
      CALL LeeDebug(FLog, PriProgDin, PDSvFl, ErSvFl, PsFzFl,           &
     &     PMSvFl, FSvLaPs, FDatChe,                                    &
     &     IndEta1Imp, IndEta2Imp, IndSimImp, IndIteImp, FBest,         &
     &     FInterfaz, ULog)

      IF (PsFzFl) THEN
         IF (FInfactSol) THEN
            WRITE(ULog,'(A,A)') 'Uso del flag PsFzFl ',                 &
     &           'es altamente no recomendado (plpdeb.mat) !!!'
            IF (FInterfaz .gt. 1) THEN
            WRITE(6,'(A,A)') 'Uso del flag PsFzFl ',                    &
     &           'es altamente no recomendado (plpdeb.mat) !!!'
               WRITE(6, '(A)') 'plpdeb.dat: Continuo?'
               READ(5, *)
               WRITE(6, '(A)') 'plpdeb.dat: Continuando...'
            ENDIF
         ELSE
            PsFzFl = .FALSE.
            WRITE(ULog,'(A,A)') 'Uso del flag PsFzFl ignorado, ',       &
     &           'requiere uso explicito de PLP_INFACTSOL_MODE'
         ENDIF
      ENDIF

!
!     Inicializacion del software de optimizacion
!************************************************

      UPreSuf = Abrir (NArcPreSuf, 'UNKNOWN', 'SEQUENTIAL', ULog)

      CALL Version(6)
      CALL Version(ULog)

!
!     Lee duracion etapas.
      CALL LeeEtaDim(ULog, Dim)

      ALLOCATE(TipoEtapa(Dim%Eta))
      ALLOCATE(FDepHidEta(Dim%Eta))
      ALLOCATE(Mes(Dim%Eta))
      ALLOCATE(Year(Dim%Eta))
      ALLOCATE(EtaDur(Dim%Eta))
      ALLOCATE(FPhi(Dim%Eta))
      ALLOCATE(NBloque(Dim%Eta))
      ALLOCATE(PDNCol(Dim%Eta))
      ALLOCATE(PDNFila(Dim%Eta))
      ALLOCATE(PXNCol(Dim%Eta))
      ALLOCATE(PXNFila(Dim%Eta))

      CALL LeeEta(NEtapa, Year, Mes, EtaDur, FactTiempo,                 &
     &     TipoEtapa, NUnTiPag, FPhi, FDepHidEta, FInterfaz, ULog, Dim)

      IF (.NOT. FDepHid) THEN
         FDepHidEta = .FALSE.
      ENDIF

!
      CALL LeeBloDim(Dim, ULog)
      ALLOCATE(BloEta(Dim%Blo))
      ALLOCATE(BloDur(Dim%Blo))
      ALLOCATE(BloInd(Dim%IBlo, Dim%Eta))

      CALL LeeBlo(NEtapa, EtaDur,                                       &
     &     NBloques, BloEta, BloDur,                                    &
     &     NBloque, BloInd,                                             &
     &     ULog, Dim)


!
!     Lee parametros matematicos.
      CALL LeeMatDim(ULog, Dim)

      CALL LeeMat(PDMaxIte, PDError, UmbIntConf,                        &
     &     PMError, Lambda, FVolFinEmb, FPreProc, CCaudFalla,           &
     &     CVertimiento, CTrasmision, FPrevia, FactTiempo,              &
     &     FSeparaFCF, FGrabaCSV, FGrabaRES,                            &
     &     FConvPGradx, FConvPVar, UmbGradX, UmbZSPF,                   &
     &     NumEtaCF, ABLMax, ABEpsilon,                                 &
     &     NEtapa, FInterfaz, ULog)

      PlaneOpenMode = 1
      CALL LeeRun(PlaneFile, PlaneIterBeg, PlaneIterEnd, &
     &     PlaneOpenMode, PlaneMaxIter, ULog)

      FPreProc = .FALSE.
      FPrevia = .FALSE.

      IF (FOnePhi) THEN
         FConvPGradx = .TRUE.
      ENDIF

      IF (FSeparaCF) THEN
         FSeparaFCF = .TRUE.
      ENDIF
!
!     Lee nombre barras.

      CALL LeeCnfBarDim(ULog, Dim)
      ALLOCATE(BarNom(Dim%Bar))

      CALL LeeCnfBar(NBarra, BarNom, ULog, Dim)
      ALLOCATE(BloPot(Dim%Bar, Dim%Blo))
!
!     Lee demandas barras.
      CALL LeeDem(FDatChe, NBloques, NBarra, BloPot, BarNom, ULog, Dim)

!
!     Lee configuracion electrica.

      CALL LeeCnfLinDim(ULog, Dim)

      ALLOCATE(LinNom(Dim%Lin))
      ALLOCATE(LinNBar(2, Dim%Lin))
      ALLOCATE(LinNFlu(Dim%Lin))
      ALLOCATE(LinFPer(Dim%Lin))
      ALLOCATE(LinHVDC(Dim%Lin))

      CALL LeeIndSimDim(ULog, Dim)

      CALL LeeManLinSDim(Dim, ULog)
      ALLOCATE(LinFOpe(Dim%Lin, Dim%Blo, Dim%LinManS))
      ALLOCATE(LinRes(Dim%Lin, Dim%Blo, Dim%LinManS))
      ALLOCATE(LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo, Dim%LinManS))
      ALLOCATE(LinVNom(Dim%Lin, Dim%Blo, Dim%LinManS))
      ALLOCATE(LinXImp(Dim%Lin, Dim%Blo, Dim%LinManS))
      ALLOCATE(LinTMaxS(2, Dim%Flu, Dim%Lin, Dim%Blo))

      CALL LeeCnfLin(                                                   &
     &     NBloques, NBarra, NLinea, FPerdTram, FPerdLin,               &
     &     LinNom, LinNBar, LinNFlu, LinFPer, ThetaRef,                 &
     &     LinFOpe, LinRes, LinTMax, LinVNom, LinXImp, LinHVDC,         &
     &     NFlujo, ULog, Dim)

!
!     Lee mantenimiento lineas.
      IF (NLinea .GT. 0) THEN
         CALL LeeManLin(FDatChe, NBloques, NLinea, LinNom, LinNFlu,     &
     &        LinFOpe, LinRes, LinTMax, LinVNom, LinXImp,               &
     &        Finterfaz, ULog, Dim)
      ENDIF
      IF ((Nlinea .EQ. 0) .AND. (NBarra .GT. 1)) THEN
         CALL SumaDem(NBloques, NBarra, BloPot, ULog, Dim)
         NBarra = 1
      ENDIF
!
      CALL LeeCnfCenDim(ULog, Dim)

      ALLOCATE(CenTipo(Dim%Cen))
      ALLOCATE(CenNom(Dim%Cen))
      ALLOCATE(CenGBar(Dim%Cen))
      ALLOCATE(CenGHid(Dim%Cen, 2))
      ALLOCATE(CenVHid(Dim%Cen, 2))
      ALLOCATE(EmbCFUE(Dim%Emb))
      ALLOCATE(CenInd(Dim%Cen))
      ALLOCATE(CenCVar(Dim%Cen, Dim%Eta))
      ALLOCATE(CenRen(Dim%Cen))
      ALLOCATE(CenVMax(Dim%Vert, Dim%Blo))
      ALLOCATE(CenVMin(Dim%Vert, Dim%Blo))
      ALLOCATE(EmbVIni(Dim%Emb))
      ALLOCATE(EmbVFin(Dim%Emb))
      ALLOCATE(EmbFEsc(Dim%Emb))
      ALLOCATE(EstocFIndep(Dim%Hid))

      CALL LeeAflCenDim(ULog, Dim)
      ALLOCATE(HidSPPQAfl(Dim%HidSPP, Dim%Blo, Dim%Clase))
      ALLOCATE(PasQAfl(Dim%Pas, Dim%Blo, Dim%Clase))


      CALL LeeManCenSDim(Dim, ULog)
      ALLOCATE(CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS))
      ALLOCATE(CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS))

      CALL LeeManEmbSDim(Dim, ULog)
      ALLOCATE(EmbVMax(Dim%Emb, Dim%Eta, Dim%EmbManS))
      ALLOCATE(EmbVMin(Dim%Emb, Dim%Eta, Dim%EmbManS))
      ALLOCATE(EmbVMaxA(Dim%Emb))


!     Lee configuracion hidraulica.
      CALL LeeCnfCen(NEtapa, NBLoques,                                  &
     &     NLinea, NCentral, NCenFalla,                                 &
     &     NCenEmb, NCenSer, NCenPas,                                   &
     &     CenNom, CenTipo, CenPMin, CenPMax, CenVMin, CenVMax,         &
     &     CenCVar, EmbCFUE,                                            &
     &     CenRen, CenGBar, CenGHid, CenVHid,                           &
     &     HidSPPQAfl, PasQAfl,                                         &
     &     EmbVIni, EmbVFin, EmbVMin, EmbVMax, EmbFEsc,                 &
     &     FactTiempo,                                                  &
     &     EstocFIndep, CenInd, FInterfaz, ULog, Dim)

      CALL LeeCenTipo(Ulog,CenTipo,CenNom,Dim)

!     Lee cota final embalse con funcion costo futuro.
      CALL VolFinEmb(NEtapa, NCenEmb, FVolFinEmb, EmbCFUE,              &
     &     EmbVFin, EmbVMin, EmbVMax, Dim)

      FProgEnt = .FALSE.
!
!     Lee costos centrales.
      CALL LeeCosCen(FDatChe, NEtapa, NCentral,                         &
     &     CenNom, CenCVar, FactTiempo, FInterfaz, ULog, Dim)
!
!     Lee mantenimiento centrales.
      CALL LeeManCen(FDatChe, NBloques, NCentral,                       &
     &     CenNom, CenPMin, CenPMax,                                    &
     &     FInterfaz, ULog, Dim)

!
!     CAUDALES discretizados en*NClase*clases para cada etapa y central


      CALL LeeAflCen(FDatChe, NBloques, NCentral, NCenEmb, NCenSer,     &
     &     NCenPas, CenNom, CenTipo,                                    &
     &     HidSPPQAfl, PasQAfl, NClase,                                 &
     &     ULog, Dim)
!
!     INDICES de los caudales discretizados en*NSimul*clases del
!     archivo leidas en la rutina _leeaflcen_.
!     estos apuntan a los caudales de cada SIMULACION.

      ALLOCATE(SimulInd(Dim%Simul, Dim%Eta))

      CALL LeeIndSim(FDatChe, NEtapa,                                   &
     &     NSimul, SimulInd, NClase,                                    &
     &     FInterfaz, ULog, Dim)

!
!     INDICES de las caudales discretizados en*NClase*clases del
!     archivo leidas en la rutina _leecaudal_.
!     Estos apuntan a los caudales de las APERTURAS de cada simulacion.
!     Esta lectura es relevante solo si el flag*FPAR*leido en la
!     rutina _leepar_ es falso.
      ALLOCATE(NApert(Dim%Simul, Dim%Eta))
      ALLOCATE(ApertInd(Dim%Apert, Dim%Simul, Dim%Eta))
      ALLOCATE(ApertInd2(Dim%Apert, Dim%Eta))
      CALL LeeIndApe(FDatChe, NEtapa,                                   &
     &     NApert, NSimul, NClase, ApertInd,                            &
     &     SimulInd, FInterfaz, ULog, Dim)
      CALL LeeIndApe2(FDatChe, NEtapa,                                  &
     &     NApert, ApertInd2,                                           &
     &     SimulInd, FInterfaz, ULog, Dim)

      NEmbVReb = 0
      IF (FVertReb) THEN
         CALL LeeVRebEmbDim(ULog, Dim)

         ALLOCATE(EmbVReb(Dim%EmbVReb))
         ALLOCATE(EmbCReb(Dim%EmbVReb))
         ALLOCATE(EmbVRebInd(Dim%EmbVReb))

         CALL LeeVRebEmb(NEmbVReb, EmbVRebInd, NCenEmb, CenNom, &
     &        EmbVReb, EmbCReb, ULog, Dim)
      ELSE
         ALLOCATE(EmbVReb(0))
         ALLOCATE(EmbCReb(0))
         ALLOCATE(EmbVRebInd(0))
      ENDIF

      IF (NEmbVReb .EQ. 0) THEN
         FVertReb = .FALSE.
      ENDIF


!
!     Lee mantenimientos embalses.
      CALL LeeManEmb(FDatChe, NEtapa, NCenEmb, CenNom, EmbFEsc,         &
     &     EmbVMin, EmbVMax, FInterfaz, ULog, Dim)


      ALLOCATE(EmbVMinHInd(Dim%Emb))
      ALLOCATE(EmbVMinH(Dim%Emb, Dim%Eta))
      ALLOCATE(EmbCMinH(Dim%Emb, Dim%Eta))

      NEmbVMinH = 0
      EmbVMinHInd(1:Dim%Emb) = 0
      EmbVMinH(1:Dim%Emb, 1:Dim%Eta) = EmbVMin(1:Dim%Emb, 1:Dim%Eta, 1)
      EmbCMinH(1:Dim%Emb, 1:Dim%Eta) = 0.0d0
      CALL LeeMinEmbH(NEtapa, NCenEmb, CenNom, EmbFEsc,         &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, ULog, Dim)


!
!     Lee mantenimientos especializados por simulacion
!
      ALLOCATE(CenManSInd(Dim%Simul))
      ALLOCATE(EmbManSInd(Dim%Simul))
      ALLOCATE(LinManSInd(Dim%Simul))

      CALL LeeManCenS(FDatChe, NBloques, NSimul, NCentral,              &
     &     CenNom, CenPMin, CenPMax,                                    &
     &     CenManSInd, FSeparaLP, FInterfaz, ULog, Dim)

!
      CALL LeeManEmbS(FDatChe, NEtapa, NSimul, NCenEmb, CenNom,         &
     &     EmbFEsc, EmbVMin, EmbVMax, EmbVMaxA,                         &
     &     EmbManSInd, FSeparaLP, FInterfaz, ULog, Dim)

!
      CALL LeeManLinS(FDatChe, NBloques, NSimul, NLinea,                &
     &     LinNom, LinNFlu,                                             &
     &     LinFOpe, LinRes, LinTMax, LinVNom, LinXImp,                  &
     &     LinTMaxS, LinManSInd, FSeparaLP, Finterfaz, ULog, Dim)

!     Configura los volumenes de rebalse por defecto

!     Lee limites de Qe de embalses

      ALLOCATE(EmbQeLow(Dim%Emb))
      ALLOCATE(EmbQeUpp(Dim%Emb))

      CALL LeeQeBnd(NCenEmb, CenNom, EmbQeLow, EmbQeUpp, ULog, Dim)

!     Lee datos filtraciones embalses.

      FFiltVar = .FALSE.
      CALL LeeFilEmbDim(FFiltVar, ULog, Dim)

      IF (FFiltVar) THEN
         ALLOCATE(FiltCenInd(0))

         ALLOCATE(CenFHid(Dim%EmbFilt))
         ALLOCATE(FiltEmbInd(Dim%EmbFilt))
         ALLOCATE(FiltNTramo(Dim%EmbFilt))
         ALLOCATE(FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam))
         ALLOCATE(FiltProm(Dim%EmbFilt, Dim%Eta + 1))
         CALL LeeFilEmb(NCentral, NCenEmb, CenNom,                         &
     &        CenFHid, FiltEmbInd,                                         &
     &        FiltNTramo, FiltParam, FiltNCen, FiltProm, EmbVIni, NEtapa,  &
     &        FFiltVar, ULog, Dim)
      ELSE
         CALL LeeCenFilDim(ULog, Dim)

         ALLOCATE(CenFHid(0))

         ALLOCATE(FiltCenInd(Dim%EmbFilt))
         ALLOCATE(FiltEmbInd(Dim%EmbFilt))
         ALLOCATE(FiltNTramo(Dim%EmbFilt))
         ALLOCATE(FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam))
         ALLOCATE(FiltProm(Dim%EmbFilt, Dim%Eta + 1))
         CALL LeeCenFil(NCentral, NCenEmb, CenNom,                      &
     &        FiltCenInd, FiltEmbInd,                                   &
     &        FiltNTramo, FiltParam, FiltNCen, FiltProm, EmbVIni,       &
     &        NEtapa, ULog, Dim)
      ENDIF

!     Lee datos extracciones embalses.
      FExtrac = .FALSE.
      CALL LeeExtrDim(FExtrac, ULog, Dim)
      ALLOCATE(CenXHid(Dim%Extr))
      ALLOCATE(ExtrCenInd(Dim%Extr))
      ALLOCATE(ExtrMax(Dim%Extr))

      ExtrNCen = 0
      IF (FExtrac) THEN
         CALL LeeExtr(NCentral, CenNom,                                    &
     &        FExtrac, ExtrNCen, ExtrMax, ExtrCenInd, CenXHid,             &
     &        ULog, Dim)
      ENDIF
      IF (ExtrNCen .EQ. 0) THEN
         FExtrac = .FALSE.
      ENDIF


!     Configura las escalas de variables de volumen
      IF (FConvLaja .EQ. 1) THEN
         Dim%PDLDAcCol = Dim%PDLDAcCol + ParLaja%DimPDLDAcCol
         Dim%PDLDAcFila = Dim%PDLDAcFila + ParLaja%DimPDLDAcFila
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         Dim%PDLDAcCol = Dim%PDLDAcCol + ParLajaM%DimPDLDAcCol
         Dim%PDLDAcFila = Dim%PDLDAcFila + ParLajaM%DimPDLDAcFila
      ENDIF

      IF (FConvMaule .NE. 0) THEN
         Dim%PDLDAcCol = Dim%PDLDAcCol + ParMaule%DimPDLDAcCol
         Dim%PDLDAcFila = Dim%PDLDAcFila + ParMaule%DimPDLDAcFila
      ENDIF

      IF (FRestGnl) THEN
         CALL LeeGnl(NEtapa,                                            &
     &        NCentral, CenNom, CenRen, CenCVar,                        &
     &        ParGnl, ULog, Dim)

         CALL LeeGn(NEtapa,                                             &
     &        NCentral, CenNom, CenRen, CenCVar,                        &
     &        ParGn, ULog, Dim)
      ENDIF

      IF (FRestGnl) THEN
         Do I = 1, ParGnl%NumTGNL
            Dim%PDLDAcCol = Dim%PDLDAcCol + ParGnl%TGNL(I)%DimPDLDAcCol
            Dim%PDLDAcFila = Dim%PDLDAcFila + ParGnl%TGNL(I)%DimPDLDAcFila
         ENDDO
      ENDIF

      ALLOCATE(ScaleVol(Dim%PDLDAcCol))
      SCaleVol = 1.0d0

!     Configura las escalas de volumenes dinamicas
      ScaleOffset = 0
      ScaleVol(1:NCenEmb) = ScaleVolG
      IF (FScaleVdi) THEN
         WRITE(ULog,*) 'Escalas de volumenes por embalse'
         DO IVol = 1, NCenEmb
            ScaleVol(IVol) = ScaleVol(IVol) * &
     &           MAX(1d0, 1d-3*EmbVMaxA(IVol))
         ENDDO
      ENDIF
      ScaleOffset = ScaleOffset + NCenEmb

      FConvNuevos = .FALSE.
      IF (FConvLaja + FConvMaule .ne. 0) THEN
         FConvNuevos = .TRUE.
      ENDIF

!     verificacion general de los convenions nuevos
      IF (FConvNuevos) THEN
         IF (.NOT. DxAEQy(CVertimiento, 0.0d0, 0.0d0)) THEN
            WRITE(ULog,*) 'Conv. de riego debe usar costo vert. cero. Forzando cero.'
            IF (FInterfaz .gt. 1) THEN
               WRITE(6,*) 'Conv. de riego debe usar costo vert. cero. Forzando cero.'
               WRITE(6, '(A)') 'plpmat.dat: Continuo?'
               READ(5, *)
               WRITE(6, '(A)') 'plpmat.dat: Continuando...'
            ENDIF
            CVertimiento = 0.0d0
         ENDIF

         IF (.NOT. FFiltVar) THEN
            WRITE(6,*) 'Error: Conv. de riego nuevo sin plpfilemb.dat'
            WRITE(ULog,*) &
     &           'Error: Conv. de riego nuevo sin plpfilemb.dat'

            STOP 1
         ENDIF
      ENDIF


      IF (FConvLaja .EQ. 1) THEN
         CALL LeeLaja('plplajan.dat', FConvLaja,                        &
     &        NCenEmb, NCenSer,                                         &
     &        CenNom, NEtapa, Mes, FactTiempo,                          &
     &        FiltNCen, FiltEmbInd, EmbVMaxA,                           &
     &        FVertReb, NEmbVReb, EmbVRebInd,                           &
     &        ScaleVol, ParLaja, ULog, Dim)
         DO Idx = 1, ParLaja%PDLDAcNCol
            SCaleVol(ScaleOffset + Idx) = ParLaja%ScaleVol
         ENDDO
         ScaleOffset = ScaleOffset + ParLaja%PDLDAcNCol
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         CALL LeeLajaM('plplajam.dat',                                  &
     &        NCenEmb, NCenSer,                                         &
     &        CenNom, NSimul, NEtapa, Mes,                              &
     &        FiltNCen, FiltEmbInd,                                     &
     &        ScaleVol, ParLajaM, ULog, Dim)
         DO Idx = 1, ParLajaM%PDLDAcNCol
            SCaleVol(ScaleOffset + Idx) = ParLajaM%ScaleVol
         ENDDO
         ScaleOffset = ScaleOffset + ParLajaM%PDLDAcNCol
      ENDIF

      IF (FConvMaule .NE. 0) THEN
         CALL LeeMaule('plpmaulen.dat', FConvMaule,                     &
     &        NCentral, NFlujo, NCenEmb,                                &
     &        CenNom, CenInd, NEtapa, Mes,                              &
     &        FiltNCen, FiltEmbInd, ScaleVol,                           &
     &        FVertReb, NEmbVReb, EmbVRebInd,                           &
     &        ParMaule, ULog, Dim)
         DO Idx = 1, ParMaule%PDLDAcNCol
            SCaleVol(ScaleOffset + Idx) = ParMaule%ScaleVol
         ENDDO
         ScaleOffset = ScaleOffset + ParMaule%PDLDAcNCol
      ENDIF

      IF (FRestRalco) THEN
         WRITE(ULog,*) 'leeralco: Se habilita restriccion Ralco'
         CALL LeeRalco('plpralco.dat',                                  &
     &        NCenEmb, CenNom, ScaleVol, ParRalco, ULog, Dim)
      ENDIF


      FConvAntiguos = .FALSE.
      IF (FConvNuevos) THEN
         MauleIPar(IIUsoConvMaule) = 0
         LajaLPar(IUsoConvLaja) = .FALSE.
         FOldLaja = .FALSE.
         FOldMaule = .FALSE.

         ALLOCATE(MesBal(0))
         ALLOCATE(Cau2Vol(0))
         ALLOCATE(CauDemNuReEta(0))
         ALLOCATE(CauDemRegAbaEta(0))
         ALLOCATE(CauDemRegTucaEta(0))
         ALLOCATE(LajaRPar(0, 0, 0))
         ALLOCATE(CauRes105Eta(0))
         ALLOCATE(CauConMauEta(0))
         ALLOCATE(MauleRPar(0, 0, 0))
         ALLOCATE(MauleLPar(0, 0, 0))

      ELSE
         ALLOCATE(MesBal(Dim%Eta))
         ALLOCATE(Cau2Vol(Dim%Eta))
         ALLOCATE(CauDemNuReEta(Dim%Eta))
         ALLOCATE(CauDemRegAbaEta(Dim%Eta))
         ALLOCATE(CauDemRegTucaEta(Dim%Eta))
         ALLOCATE(LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1))
         ALLOCATE(CauRes105Eta(Dim%Eta))
         ALLOCATE(CauConMauEta(Dim%Eta))
         ALLOCATE(MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta))
         ALLOCATE(MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta))

         CALL Maule0(Cau2Vol,                                           &
     &        MauleIPar, MauleCPar, MauleLPar, MauleRPar,               &
     &        Mes, CauConMauEta, CauRes105Eta,                          &
     &        CenNom, CenGHid, CenVHid, CenInd,                         &
     &        NBloque, BloInd,                                          &
     &        CenManSInd, CenPMin, CenPMax, CenVMin, CenVMax,           &
     &        NEtapa, NCentral,                                         &
     &        NSimul, NFlujo, NCenEmb, NCenSer, NCenPas,                &
     &        EtaDur, EmbVIni, FactTiempo, Year, Ulog, Dim)
         FOldMaule = MauleIPar(IIUsoConvMaule) .NE. 0
         IF (FOldMaule) THEN
            CALL LeeExtMau(FDatChe, MauleIPar, MauleCPar,               &
     &           NEtapa, NClase, Ulog, Dim)
         ENDIF
         LajaCPar%UDebLog = Abrir (NArcDebLog, 'UNKNOWN', 'SEQUENTIAL', &
     &        ULog)
         CALL Laja0(Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,    &
     &        NBloque, BloInd, BloDur,                                  &
     &        Mes, MesBal, CauDemNuReEta, CauDemRegAbaEta,              &
     &        CauDemRegTucaEta,                                         &
     &        CenNom, NEtapa, NCentral,                                 &
     &        NSimul, NFlujo, NCenEmb, NCenSer,                         &
     &        CenManSInd, CenPMax,                                      &
     &        EtaDur, EmbVIni, FactTiempo, Year,                        &
     &        HidSPPQAfl, FiltProm,                                     &
     &        SimulInd, Ulog, Dim)
         FOldLaja = LajaLPar(IUsoConvLaja)
         IF (FOldLaja .OR. FOldMaule) THEN
            FConvAntiguos = .TRUE.
         ENDIF
      ENDIF

!     verificacion general de los convenions antigus
      IF (FConvAntiguos) THEN
         IF (DxAEQy(CVertimiento, 0.0d0, 0.0d0)) THEN
            WRITE(ULog,*) 'Conv. de riego ant. requiere costo vert.'
            IF (FInterfaz .gt. 1) THEN
            WRITE(6,*) 'Conv. de riego ant. requiere costo vert.'
               WRITE(6, '(A)') 'plpmat.dat: Continuo?'
               READ(5, *)
               WRITE(6, '(A)') 'plpmat.dat: Continuando...'
            ENDIF
         ENDIF
         IF (FFiltVar) THEN
            WRITE(6,*) 'Error: Conv. de riego ant. con plpfilemb.dat'
            WRITE(ULog,*) 'Error: Conv. de riego ant. con plpfilemb.dat'
            STOP 1
         ENDIF
      ENDIF

!     ultima verificacion de coexistencia de convenios
      IF (FConvNuevos .AND. FConvAntiguos) THEN
         WRITE(6,*) 'Conv. de riego antiguos y nuevos habilitados'
         WRITE(ULog,*) 'Conv. de riego antiguos y nuevos habilitados'
         STOP 1
      ENDIF

!
!     Lee planos de costo futuro embalses.
      CALL LeePlaEmbDim(NumEtaCF, ULog, Dim)

      ALLOCATE(PlaCFBeta0(Dim%XFila))
      ALLOCATE(PlaCFRho(Dim%PDLDAcCol, Dim%XFila))
      ALLOCATE(PlaCFIndCol(Dim%XFila))

      CALL LeePlaEmb(NCenEmbCFUE, NCenEmb,                              &
     &     PlaCFNCol, PlaCFNFila, EmbCFUE,                              &
     &     CenNom, PlaCFRho, PlaCFBeta0, PlaCFIndCol, PlaCFFO,          &
     &     NumEtaCF, FInterfaz, ULog, Dim)

!
!     Lee datos rendimientos embalses.
      CALL LeeCenRenDim(ULog, Dim)

      ALLOCATE(RendCenInd(Dim%EmbRend))
      ALLOCATE(RendEmbInd(Dim%EmbRend))
      ALLOCATE(RendNTramo(Dim%EmbRend))
      ALLOCATE(RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam))
      ALLOCATE(RendProm(Dim%EmbRend, Dim%Eta + 1))

      CALL LeeCenRen(NCentral, NCenEmb, CenNom,                         &
     &     RendCenInd, RendEmbInd,                                      &
     &     RendNTramo, RendParam, RendNCen, RendProm, EmbVIni, NEtapa,  &
     &     ULog, Dim)

!     Lee datos pmax embalses.
      CALL LeeCenPmaxDim(ULog, Dim)

      ALLOCATE(PmaxCenInd(Dim%EmbPmax))
      ALLOCATE(PmaxEmbInd(Dim%EmbPmax))
      ALLOCATE(PmaxNTramo(Dim%EmbPmax))
      ALLOCATE(PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam))

      CALL LeeCenPmax(NCentral, NCenEmb, CenNom,                        &
     &     PmaxCenInd, PmaxEmbInd,                                      &
     &     PmaxNTramo, PmaxParam, PmaxNCen,                             &
     &     ULog, Dim)

      IF (PmaxNCen .gt. 0) THEN
         FRendBdrs = .TRUE.
         FRendProm = .FALSE.
      ENDIF
!
! ######### Lee Baterias ########
!
!  Esta flag es siempre positiva a exepción que se explicite lo contrario
!  (revisar GET_OPTS)
   IF (FBaterias) THEN
      ! Verifica que exista el archivo, y consigue las dimensiones
      CALL LeeCenBatDim(ParBaterias,FBaterias,ULog,Dim)
      ! Si no esta el archivo, no entra a este condicional
      IF (FBaterias) THEN
         ! Lee el archivo de baterias
         CALL LeeCenBat(ParBaterias,CenNom,Dim,ULog)
         ! Lee mantenimientos de energia
         CALL LeeManBat(ParBaterias,Dim, ULog)
      ENDIF
   ENDIF

      IF (FRestReserva) THEN
         CALL LeeCnfRes(                                        &
     &        NCentral, CenNom, FactTiempo,                     &
     &        NBloques, RendNCen, RendCenInd,                   &
     &        FRestReserva, ParRes, ULog, Dim, FConvLaja)

         IF (FRestReserva) THEN
            CALL LeeManResc(NBloques, ParRes, ULog)
            CALL LeeManResz(NBloques, ParRes, ULog)
            CALL LeeCosResc(NEtapa, FactTiempo, ParRes, ULog)
            CALL LeeManResg(NSimul, NBloques, ParRes, ULog)
            IF (FMemMode .eq. -1) THEN
               FMemMode = -2
            ENDIF
         ENDIF
      ENDIF

!
      LRec = 8
      USimPrimal0 = AbrirScratch(LRec, ULog)
      USimPrimal1 = AbrirScratch(LRec, ULog)
      USimDual0 = AbrirScratch(LRec, ULog)
      USimDual1 = AbrirScratch(LRec, ULog)
      IF ((USimPrimal0 .EQ. 0) .OR.                                     &
     &     (USimPrimal1 .EQ. 0) .OR.                                    &
     &     (USimDual0 .EQ. 0) .OR.                                      &
     &     (USimDual1 .EQ. 0)) THEN
         WRITE (6, '(A)')                                               &
     &        'plp-main: Error al abrir ''sp?'' o ''sd?''.'
         WRITE (ULog, '(A)')                                            &
     &        'plp-main: Error al abrir ''sp?'' o ''sd?''.'
         STOP 1
      ENDIF

!     Numero de Componentes del Kit
!     *****************************
      ALLOCATE(Kit(Dim%Simul, Dim%Kit))
      DO ISimul = 1, NSimul
         Kit(ISimul, ISPFA) = USimPrimal0
         Kit(ISimul, ISDFA) = USimDual0
         IF (FBest) THEN
            Kit(ISimul, IMSPFHM) = USimPrimal1
            Kit(ISimul, IMSDFHM) = USimDual1
         ELSE
            Kit(ISimul, IMSPFHM) = USimPrimal0
            Kit(ISimul, IMSDFHM) = USimDual0
         ENDIF
      ENDDO
!     Incializa Indices de coeficientes de matriz PD
!
      IF (FExtrac) THEN
         ALLOCATE(ExtrColInd(Dim%Extr, Dim%IBlo, Dim%Eta))
      ELSE
         ALLOCATE(ExtrColInd(0,0,0))
      ENDIF

      ALLOCATE(EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta))
      ALLOCATE(EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta))

      ALLOCATE(FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta))
      ALLOCATE(FiltVarColInd(Dim%EmbFilt, Dim%Eta))
      ALLOCATE(FiltVarFilInd(Dim%EmbFilt, Dim%Eta))

      ALLOCATE(RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta))
      ALLOCATE(RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta))
      ALLOCATE(RendVolColInd(Dim%EmbRend, Dim%Eta))

      ALLOCATE(PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta))
      ALLOCATE(PmaxVolColInd(Dim%EmbPmax, Dim%Eta))

      ALLOCATE(EmbVRebFilInd(Dim%EmbVReb, Dim%Eta))

      ALLOCATE(PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta))
      ALLOCATE(PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta))

!
!
      CALL PRINT_OPTS(ULog, &
     &     eprhs, &
     &     epopt, &
     &     ScaleObj, &
     &     ScalePhi, &
     &     ScaleAng, &
     &     ScaleVolG, &
     &     FScaleQs, &
     &     FScaleVdi, &
     &     FAngZero, &
     &     FConvLaja, &
     &     FConvMaule, &
     &     FRestRalco, &
     &     FRestGnl, &
     &     FRestReserva, &
     &     FBaterias, &
     &     FMemMode, &
     &     FOldLaja, &
     &     FOldMaule, &
     &     FVertReb, &
     &     FSeparaLP, &
     &     FSeparaCF, &
     &     FZSPFBest, &
     &     FOnePhi, &
     &     FInterfaz, &
     &     FDepHid, &
     &     FlagFilt, &
     &     FFiltVar, &
     &     FExtrac, &
     &     FRendProm, &
     &     FRendBdrs, &
     &     FAfluFict, &
     &     FactEPS, &
     &     FactMLD, &
     &     OptiEPS, &
     &     OptiMLD, &
     &     FactMXC, &
     &     FactDBL, &
     &     FSeparaCFA, &
     &     FOneFeasRay, &
     &     DualFactIter, &
     &     FInfactSol, &
     &     idual_pmode, &
     &     iprim_pmode, &
     &     nthreads, &
     &     CenNom, ScaleVol, &
     &     Dim)
!

      CALL GenMatPD(FAfluFict,                                          &
     &     FVertReb, NEmbVReb, EmbVRebFilInd,                           &
     &     NEmbVMinH,                                                   &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     FOldLaja, LajaCPar, LajaIPar,                                &
     &     FOldMaule, MauleCPar, MauleIPar,                             &
     &     NEtapa, NBarra, NLinea, NFlujo, NCentral,                    &
     &     NCenEmb, NCenSer, NCenPas,                                   &
     &     NBloque,                                                     &
     &     CenGBar,                                                     &
     &     PDNFila, PDNCol,                                             &
     &     PDLDAcNFila,                                                 &
     &     PDLDAcNCol, PDLDAcFilaInd, PDLDAcColInd,                     &
     &     FiltCenInd,                                                  &
     &     FiltColInd, FiltNCen,                                        &
     &     FFiltVar, FiltVarColInd, FiltVarFilInd,                      &
     &     FExtrac, ExtrNCen, ExtrCenInd, ExtrColInd, ExtrMax,          &
     &     RendCenInd, RendEmbInd,                                      &
     &     RendColInd, RendFilaInd, RendVolColInd, RendNCen,            &
     &     PmaxCenInd, PmaxEmbInd,                                      &
     &     PmaxColInd, PmaxVolColInd, PmaxNCen,                         &
     &     EstocNCol, EstocNFila, EstocColInd, EstocFilaInd,            &
     &     NSimul, NVarPhi,                                             &
     &     Dim)

      CALL GenMatX(PXNCol, PXNFila, NEtapa,                             &
     &     PlaCFNCol, PlaCFNFila)

      NCenRes = ParRes%NumCen
      NZonRes = ParRes%NumZonas
      CALL PriCas(NCentral, NCenEmb, NCenSer, NCenPas,                  &
     &     NBarra, NLinea, NBloques, NEtapa, NCenFalla,                 &
     &     NCenEmbCFUE, NSimul, FPerdTram, NCenRes, NZonRes, 6)
      CALL PriCas(NCentral, NCenEmb, NCenSer, NCenPas,                  &
     &     NBarra, NLinea, NBloques, NEtapa, NCenFalla,                 &
     &     NCenEmbCFUE, NSimul, FPerdTram, NCenRes, NZonRes, ULog)

      ALLOCATE(AIncid(Dim%HidSPP, 0:Dim%HidSPP))
      ALLOCATE(envs(Dim%Simul))
      ALLOCATE(lp(Dim%Simul, Dim%Eta))

      envs = 0
      lp = 0
      DO i = 1, NSimul
         CALL IniSvrEnv(envs(i), epopt, eprhs, FMemMode)
      ENDDO

      IF (nthreads .GT. 1) THEN
         IF (.NOT. FSeparaLP) THEN
            FSeparaLP = .TRUE.
            WRITE(6,*) 'Activando flag FSeparaLP req. para Paralelismo.'
         ENDIF
      ENDIF

      CALL DefPrbPD(NEtapa, NSimul, FOnePhi, FSeparaLP,                 &
     &     FAngZero, FPerdTram, FPerdLin,                               &
     &     FAfluFict, FScaleQs,                                         &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVReb, EmbCReb,            &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH,                  &
     &     ScaleObj, ScalePhi, ScaleAng, ScaleVol, OptiEPS, OptiMLD,    &
     &     CCaudFalla, CVertimiento, CTrasmision,                       &
     &     NBarra, NLinea, NFlujo, NCentral,                            &
     &     NCenEmb, NCenSer, NCenPas, CenInd,                           &
     &     NBloque, BloInd, BloDur, BloPot,                             &
     &     CenManSInd, LinManSInd, EmbManSInd,                          &
     &     CenPMin, CenPMax,                                            &
     &     CenVMin, CenVMax,                                            &
     &     CenCVar, CenRen, CenGBar, CenGHid, CenVHid,                  &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd, FiltVarColInd, CenFHid,      &
     &     FExtrac, ExtrNCen, ExtrCenInd, ExtrMax, CenXHid,             &
     &     EmbVIni, EmbVMin, EmbVMax,                                   &
     &     EmbQeLow, EmbQeUpp,                                          &
     &     LinVNom, LinRes, LinXImp, LinTMax,                           &
     &     LinNBar, LinNFlu, LinFPer, LinFOpe, LinHVDC,                 &
     &     FPhi, FactTiempo,                                            &
     &     PDNFila, PDNCol,                                             &
     &     PXNCol, PXNFila, PDLDAcNCol, PDLDAcColInd,                   &
     &     PlaCFRho, PlaCFBeta0, PlaCFIndCol,                           &
     &     NVarPhi, PlaneMaxIter,                                       &
     &     FSeparaFCF, FDepHidEta,                                      &
     &     AIncid, ULog, lp, envs, nthreads, Dim)

      CALL TablRel(NEtapa, NBloques,                                    &
     &     NBarra, NCenEmb,                                             &
     &     NCentral, NLinea,                                            &
     &     CenGBar, LinNBar, AIncid, CenRen,                            &
     &     BarNom, CenNom, LinNom, LinVNom, NumEtaCF,                   &
     &     CenTipo, CenCVar, EmbVMin, EmbVMax, EmbFEsc,                 &
     &     NSimul,                                                      &
     &     BloDur, BloEta, Year, Mes, Dim, ULog)

      ALLOCATE(ZSPFArr(Dim%Simul))
      ALLOCATE(ZSPFBestArr(Dim%Simul))


      ZSPFArr = 0.0d0
      kappa = 0.0d0
      CALL ProgDin(NEtapa, NCenEmb,                                     &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,      &
     &     FScaleQs, ScaleObj, ScalePhi, ScaleVol,                      &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     PDNCol, PDNFila, PDMaxIte,                                   &
     &     UmbIntConf, PDError, PriProgDin,                             &
     &     PDSvFl, ErSvFl, PsFzFl, FSvLaPs,                             &
     &     PXNFila, PXNCol, Kit,                                        &
     &     ZSPFArr, ZSPFBestArr,                                        &
     &     ZSDF, ZSPFPromBest, PDLDAcNFila, PDLDAcNCol,                 &
     &     PDLDAcFilaInd, PDLDAcColInd,                                 &
     &     CenPMin, CenPMax, CenRen, CenManSInd, EmbVIni,               &
     &     HidSPPQAfl, PasQAfl,                                         &
     &     SimulInd, ApertInd, ApertInd2,                               &
     &     EstocFIndep, FDepHidEta, NSimul, NumEtaCF,                   &
     &     FConvPGradx, FConvPVar, UmbGradX, UmbZSPF,                   &
     &     IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,                &
     &     NApert, EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,    &
     &     FiltColInd, FiltEmbInd, FiltNCen,                            &
     &     FiltNTramo, FiltParam, FiltProm,                             &
     &     FFiltVar, FiltVarFilInd,                                     &
     &     RendColInd, RendFilaInd, RendEmbInd, RendNCen,               &
     &     RendCenInd, RendNTramo, RendParam, RendProm,                 &
     &     PmaxColInd, PmaxEmbInd, PmaxNCen,                            &
     &     PmaxCenInd, PmaxNTramo, PmaxParam,                           &
     &     Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,             &
     &     MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal,          &
     &     CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,            &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     FZSPFBest, FOnePhi, FSeparaFCF, FSeparaLP,                   &
     &     FlagFilt, FRendProm, FRendBdrs,                              &
     &     FGrabaRES, CenNom, UPreSuf, FInterfaz,                       &
     &     FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &     DualFactIter, OptiEPS, OptiMLD,                              &
     &     PlaneFile, PlaneIterBeg, PlaneIterEnd, PlaneOpenMode,        &
     &     FLog, ULog, ULogCF, lp, nthreads, kappa, Dim)

      SFVFOBest = ZSPFPromBest
      WRITE(6, '(A, 1X, F15.2, A, 1X, G11.4)')    &
     &     'plp-main: SFVFOBest = ', SFVFOBest,   &
     &     ', kappa = ', kappa

      WRITE(ULog, '(A, 1X, F15.2, A, 1X, G11.4)')    &
     &     'plp-main: SFVFOBest = ', SFVFOBest,   &
     &     ', kappa = ', kappa


      CALL GraDatS(NEtapa, NSimul,                                      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd,                              &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH,                                                   &
     &     FExtrac, ExtrNCen, ExtrCenInd,                               &
     &     NBloque, BloInd, BloDur, BloEta, FactTiempo,                 &
     &     FAfluFict, FScaleQs, ScaleVol,                               &
     &     TipoEtapa, FPhi,                                             &
     &     NBarra, NLinea, NCentral, NCenEmb, NCenSer,                  &
     &     NCenPas, FPerdTram, FPerdLin,                                &
     &     BloPot, CenNom, CenInd, CenTipo, BarNom,                     &
     &     LinNom, LinNBar, LinFPer, LinNFlu,                           &
     &     CenRen, CenGBar, CenCVar,                                    &
     &     CenPMin,CenPMax, CenManSInd,                                         &
     &     EmbVIni, EmbFEsc,                                            &
     &     LinManSInd,                                                  &
     &     LinRes, LinTMax, LinTMaxS, LinVNom,                          &
     &     NFlujo, FRendProm, RendProm, RendNCen,                       &
     &     RendCenInd, RendEmbInd, RendNTramo, RendParam,               &
     &     PmaxNCen, PmaxCenInd, PmaxEmbInd, PmaxNTramo, PmaxParam,     &
     &     Mes, Cau2Vol, LajaLPar, LajaRPar,                            &
     &     MauleIPar, MauleRPar,                                        &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     AIncid, FGrabaCSV, FGrabaRES, UPreSuf,                       &
     &     SimulInd,                                                    &
     &     HidSPPQAfl, Kit, PasQAfl, PDNCol, PDNFila,                   &
     &     FInterfaz, FWarningFalla, nthreads, ULog, Dim)

      CALL LeeTmp(TmpFin)
      CALL DifTmp(TmpFin, TmpIni, HorPro, MinPro, SegPro)
      WRITE(6, '(A, I4, A6, I2, A6, I2, A5)') 'plp-main: Tiempo ',      &
     &     HorPro, ' hor. ', MinPro, ' min. ', SegPro, ' seg.'
      WRITE(ULog, '(A, I4, A6, I2, A6, I2, A5)') 'plp-main: Tiempo ',   &
     &     HorPro, ' hor. ', MinPro, ' min. ', SegPro, ' seg.'

!     Aviso existencia generacion de falla
      IF (FWarningFalla) THEN
         WRITE(ULog, '(A)')                                             &
     &   'plp-main: Existe generacion de falla, revisar plpfal.csv.'
      ENDIF

      WRITE(6, '(A)') 'plp-main: Corrida exitosa.'
      WRITE(ULog, '(A)') 'plp-main: Corrida exitosa.'


      END
