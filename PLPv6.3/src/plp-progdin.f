!*****************************************
!     Subrutina Algoritmo Programacion Dinamica
!*****************************************
      SUBROUTINE ProgDin(NEtapa, NCenEmb,                               &
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
     &     CenPMin, CenPMax, CenRen, CenManSInd, VolEmbIni,             & 
     &     EstocRHSP, EstocUBP,                                         &
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
     &     FlagFilt, FlagRendProm, FRendBdrs,                           &
     &     FGrabaRES, PDLDAcNom, UPreSuf, FInterfaz,                    &
     &     FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &     DualFactIter, OptiEPS, OptiMLD,                              &
     &     PlaneFile, PlaneIterBeg, PlaneIterEnd,  PlaneOpenMode,       &
     &     FLog, ULog, ULogCF, lp, nthreads, kappa, Dim)

      USE PLP
      TYPE(PAR_DIMS), INTENT(IN):: Dim

      CHARACTER*24 PlaneFile
      INTEGER PlaneIterBeg, PlaneIterEnd, PlaneOpenMode
      
      INTEGER UPlane, Abrir
      
!     Convenios antiguos
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION CauDemRegAbaEta(Dim%Eta)
      DOUBLE PRECISION CauDemRegTucaEta(Dim%Eta)
      DOUBLE PRECISION CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION CauConMauEta(Dim%Eta)
      INTEGER MauleIPar(DimIMaule)
      TYPE(PAR_MAULEC) MauleCPar
      INTEGER LajaIPar(DimILaja)
      TYPE(PAR_LAJAC) LajaCPar
      LOGICAL LajaLPar(DimLLaja)
      LOGICAL MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)
      LOGICAL MesBal(Dim%Eta)
      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
!     Convenios antiguos

      CHARACTER*23 MensajeProgreso
      CHARACTER*48 PDLDAcNom(Dim%PDLDAcCol)
      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION VolEmbIni(Dim%Emb)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION EstocUBP(Dim%EstocCol, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION GetInfty
      DOUBLE PRECISION Infinity
      DOUBLE PRECISION LDPhiPrv(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)      
      DOUBLE PRECISION GradxPhiECF(Dim%PDLDAcCol, Dim%Simul)      

      DOUBLE PRECISION PDError
      DOUBLE PRECISION UmbGradX
      DOUBLE PRECISION UmbZSPF
      DOUBLE PRECISION UmbIntConf
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION ZSPFArr(Dim%Simul)
      DOUBLE PRECISION ZSPFBestArr(Dim%Simul)
      DOUBLE PRECISION ZSPFPromBest
      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      LOGICAL FSeparaCFA, FOneFeasRay
      INTEGER DualFactIter
      INTEGER FactMXC
      DOUBLE PRECISION kappa

      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION EtaDur(Dim%Eta)
      DOUBLE PRECISION FactTiempo
      INTEGER Mes(Dim%Eta)
      INTEGER Year(Dim%Eta)

      LOGICAL FOnePhi
      LOGICAL FSeparaLP
      INTEGER FInterfaz
      LOGICAL FScaleQs
      LOGICAL FZSPFBest
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleVol(Dim%Emb)
      LOGICAL FVertReb
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)
      INTEGER EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)

      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)

      INTEGER FConvLaja
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_LAJAM) ParLajaM
      INTEGER FConvMaule
      TYPE(PAR_MAULE) ParMaule
      LOGICAL FRestRalco
      TYPE(PAR_RALCO) ParRalco
      LOGICAL FRestGnl
      TYPE(PAR_GNL) ParGnl
      LOGICAL FRestReserva
      TYPE(PAR_GNL) ParRes
      LOGICAL FBaterias
      TYPE(PAR_BATERIAS) ParBaterias


      LOGICAL FFiltVar
      INTEGER FiltVarFilInd(Dim%EmbFilt, Dim%Eta)

      INTEGER ApertInd(Dim%Apert, Dim%Simul, Dim%Eta)
      INTEGER ApertInd2(Dim%Apert, Dim%Eta)

      INTEGER ArchContD(Dim%Apert, Dim%Eta, Dim%Simul)
      INTEGER ArchContP(Dim%Eta, Dim%Simul)

      INTEGER nthreads
      LOGICAL FDepHidEta(Dim%Eta)
      LOGICAL FGrabaRES
      INTEGER EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER EstocNCol
      INTEGER EstocNFila
      INTEGER FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER FiltNCen
      INTEGER FiltNTramo(Dim%EmbFilt)
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER HorPro
      INTEGER IndEta1Imp
      INTEGER IndEta2Imp
      INTEGER IndIteImp
      INTEGER IndSimImp
      INTEGER ISimul
      INTEGER IStat
      INTEGER ITemp
      INTEGER Kit(Dim%Simul, Dim%Kit)
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)
      INTEGER MinPro
      INTEGER NApert(Dim%Simul, Dim%Eta)
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NSimul
      INTEGER NumEtaCF
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      INTEGER PDLDAcNCol
      INTEGER PDLDAcNFila
      INTEGER PDMaxIte
      INTEGER PDNCol(Dim%Eta)
      INTEGER PDNFila(Dim%Eta)
      INTEGER PDNumIte
      INTEGER PXNCol(Dim%Eta)
      INTEGER PXNFila(Dim%Eta)

      DOUBLE PRECISION RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION RendProm(Dim%EmbRend, Dim%Eta + 1)
      INTEGER RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendNCen
      INTEGER RendNTramo(Dim%EmbRend)
      INTEGER RendEmbInd(Dim%EmbRend)
      INTEGER RendCenInd(Dim%EmbRend)

      DOUBLE PRECISION PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)
      INTEGER PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER PmaxNCen
      INTEGER PmaxNTramo(Dim%EmbPmax)
      INTEGER PmaxEmbInd(Dim%EmbPmax)
      INTEGER PmaxCenInd(Dim%EmbPmax)
      

      INTEGER SegPro
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER TmpFin(DimTmp)
      INTEGER TmpIni(DimTmp)

      INTEGER UWritePhi
      INTEGER UWritePhi2
      INTEGER IREC2
      INTEGER UPreSuf
      INTEGER ULog
      INTEGER ULogCF
      LOGICAL EstocFIndep(Dim%Hid)
      LOGICAL FConvPGradX
      LOGICAL FConvPVar
      LOGICAL FLog
      LOGICAL FSeparaFCF
      INTEGER FlagFilt
      LOGICAL FlagRendProm
      LOGICAL FRendBdrs
      LOGICAL PDConvrg
      LOGICAL PDConvrgVal
      LOGICAL PDSvFl
      LOGICAL ErSvFl
      LOGICAL PsFzFl
      LOGICAL FSvLaPs
      LOGICAL PriProgDin
      INTEGER FactDBL
      INTEGER MaxApert

      LOGICAL FLastPass

      DOUBLE PRECISION EmbVIni(Dim%Emb, Dim%Simul, Dim%Eta)
      INTEGER NumEmb

      INTEGER nplanes
      INTEGER PlaneIterMax

!     codigo:
      Infinity = GetInfty()

      NumEmb = NCenEmb
      DO ISimul = 1, NSimul
         EmbVIni(1:NumEmb, ISimul, 1) = VolEmbIni(1:NumEmb)
      ENDDO
      
!
!     inicializa todos los PL. esto consiste en
!     eliminar las aproximaciones de la funcion varphi(.) y fijar la
!     variable*varphi*en 0.

      CALL LoadPlanes(PlaneFile, PlaneIterBeg, PlaneIterEnd, &
     &     PlaneIterMax, nplanes, &
     &     lp, FSeparaLP, Dim, ulog)

      PDNumIte = 0
      PDNumIte = PDNumIte + PlaneIterMax
      PDMaxIte = PDMaxIte + PlaneIterMax
            
      IF (nplanes .eq. 0 .and. .NOT. FOnePhi) THEN
         CALL PDInic(NEtapa, NSimul, PDNCol, PDNFila, PXNFila,   &
     &        PXNCol, lp, Dim)
      ENDIF
!

      UPlane = 0
      IF (PlaneOpenMode .eq.  1) THEN
         UPlane = Abrir (NArcPlane, 'UNKNOWN', 'APPEND', ULog)
      ELSE IF (PlaneOpenMode .eq.  2) THEN
         UPlane = Abrir (PlaneFile, 'UNKNOWN', 'SEQUENTIAL', ULog)
      ENDIF

      ZSPFPromBest = Infinity
      ZSPFBestArr(1:NSimul) = Infinity
      ZSDF = -Infinity

      UWritePhi = 0
      UWritePhi2  = 0

      CALL PDTit(PriProgdin, NSimul, 6)
      CALL PDTit(PriProgdin, NSimul, ULog)

      CALL GraDatPlaEmb0(PDLDAcNCol, PDLDAcNom, UPreSuf, Dim)
      
      PDConvrgVal = PDNumIte .ge. PDMaxIte
      
      MaxApert = MAXVAL(NApert(1:NSimul, 1:NEtapa))
      DO WHILE (.NOT. PDConvrgVal)
         ArchContP(1:NEtapa, 1:NSimul) = 0
         ArchContD(1:MaxApert, 1:NEtapa, 1:NSimul) = 0
         kappa = 0.0d0
         CALL LeeTmp(TmpIni)
         MensajeProgreso =                                              &
     &        'progreso: fase primal: '
         FLastPass = .FALSE.
         CALL FasePrim (IStat, PDNumIte, NEtapa, NCenEmb,               &
     &        NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,   &
     &        FScaleQs, ScaleObj, ScalePhi, ScaleVol,                   &
     &        FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,   &
     &        FConvLaja, ParLaja, ParLajaM,                             &
     &        FConvMaule, ParMaule,                                     &
     &        FRestRalco, ParRalco,                                     &
     &        FRestGnl, ParGnl,                                         &
     &        FRestReserva, ParRes,                                     &
     &        FBaterias, ParBaterias,                                   &
     &        PDNCol, PDNFila, ZSPFArr,                                 &
     &        PDSvFl, ErSvFl, PsFzFl,                                   &
     &        SimulInd,                                                 &
     &        PDLDAcFilaInd, PDLDAcColInd, EstocRHSP, EstocUBP,         &
     &        CenPMin, CenPMax, CenRen,                                 &
     &        CenManSInd, EmbVIni, NSimul,                              &
     &        EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,         &
     &        Kit,                                                      &
     &        FiltColInd, FiltEmbInd, FiltNCen,                         &
     &        FiltNTramo, FiltParam, FiltProm,                          &
     &        FFiltVar, FiltVarFilInd,                                  &
     &        RendColInd, RendFilaInd, RendEmbInd, RendNCen,            &
     &        RendCenInd,  RendNTramo, RendParam, RendProm,             &
     &        PmaxColInd, PmaxEmbInd, PmaxNCen,                         &
     &        PmaxCenInd, PmaxNTramo, PmaxParam,                        &
     &        IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,             &
     &        Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,          &
     &        MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal,       &
     &        CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,         &
     &        CauConMauEta, CauRes105Eta,                               &
     &        MensajeProgreso,                                          &
     &        FLastPass,                                                & 
     &        FOnePhi, FSeparaFCF, FDepHidEta,                          &
     &        FlagFilt, FlagRendProm, FRendBdrs, FInterfaz,             &
     &        FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &        ArchContP, ULog, ULogCF, lp, nthreads, kappa, Dim, UPlane)

         IF (IStat .NE. 1) THEN
            RETURN
         ENDIF

         CALL PDBestSoln (ZSPFArr, ZSPFPromBest, NSimul, FZSPFBest)
         PDConvrgVal = PDConvrg (PDNumIte, PDMaxIte, ZSPFPromBest,      &
     &     NSimul, ZSPFArr, ZSDF, UmbIntConf, PDError,                  &
     &     FConvPGradx, FConvPVar, UmbGradX, UmbZSPF,                   &
     &     GradxPhi, GradxPhiECF, NumEtaCF, PDLDAcNCol, NEtapa, Dim)
         IF (.NOT. PDConvrgVal) THEN
            MensajeProgreso =                                           &
     &           'progreso:   fase dual: '
            CALL FaseDual (IStat, PDNumIte, NEtapa, NCenEmb,            &
     &           NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,&
     &           FScaleQs, ScaleObj, ScalePhi, ScaleVol,                &
     &           FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,&
     &           FConvLaja, ParLaja, ParLajaM,                          &
     &           FConvMaule, ParMaule,                                  &
     &           FRestRalco, ParRalco,                                  &
     &           FRestGnl, ParGnl,                                      &
     &           FRestReserva, ParRes,                                     &
     &           FBaterias, ParBaterias,                                &
     &           PDNCol, PDNFila, PXNCol,                               &
     &           ZSDF,                                                  &
     &           GradxPhi, LDPhiPrv, ApertInd, ApertInd2,               &
     &           EstocFIndep, FDepHidEta,                               &
     &           PDSvFl, ErSvFl, PsFzFl,                                &
     &           PDLDAcNFila, PDLDAcNCol, PDLDAcFilaInd, PDLDAcColInd,  &
     &           EstocRHSP, EstocUBP, CenPMin, CenPMax, CenRen,         &
     &           CenManSInd, EmbVIni,                                   &  
     &           NSimul, NApert, EstocNFila, EstocFilaInd,              &
     &           EstocNCol, EstocColInd,                                &
     &           Kit,                                                   &
     &           FiltColInd, FiltEmbInd, FiltNCen,                      &
     &           FiltNTramo, FiltParam, FiltProm,                       &
     &           FFiltVar, FiltVarFilInd,                               &
     &           RendColInd, RendFilaInd, RendEmbInd, RendNCen,         &
     &           RendCenInd, RendNTramo, RendParam, RendProm,           &
     &           PmaxColInd, PmaxEmbInd, PmaxNCen,                      &
     &           PmaxCenInd, PmaxNTramo, PmaxParam,                     &
     &           IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,          &
     &           Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,       &
     &           MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal,    &
     &           CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,      &
     &           CauConMauEta, CauRes105Eta,                            &
     &           MensajeProgreso,                                       &
     &           FOnePhi, FSeparaFCF, FSeparaLP,                        &
     &           FlagFilt, FlagRendProm, FRendBdrs, FInterfaz,          &
     &           SimulInd,                                              &
     &           FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &           DualFactIter, OptiEPS, OptiMLD,                        &
     &           ArchContD, ArchContP, ULog,ULogCF, lp, nthreads, kappa,&
     &           Dim, UPlane)

            IF (IStat .NE. 1) RETURN

            CALL GraDatPlaEmb(PDNumIte + 1, UWritePhi, ScaleVol,        &
     &           GradxPhi, LDPhiPrv, NSimul, PDLDAcNCol, NEtapa, Dim,   &
     &           Ulog)
            
            IF (FGrabaRES) THEN
               CALL GraDatPlaEmb2(IREC2, UWritePhi2, ScaleVol,          &
     &              GradxPhi, LDPhiPrv,                                 &
     &              NSimul, PDLDAcNCol, NEtapa, Dim,                    &
     &              ULog)
            ENDIF

         ENDIF
         PDNumIte = PDNumIte + 1
         CALL LeeTmp(TmpFin)
         CALL DifTmp(TmpFin, TmpIni, HorPro, MinPro, SegPro)
         CALL PDLin(PriProgDin, PDNumIte, ZSPFArr, ZSPFPromBest,        &
     &        ZSDF, kappa, NSimul, HorPro, MinPro, SegPro, 6)
         CALL PDLin(PriProgDin, PDNumIte, ZSPFArr, ZSPFPromBest,        &
     &        ZSDF, kappa, NSimul, HorPro, MinPro, SegPro, ULog)
         CALL PDLin(PriProgDin, PDNumIte, ZSPFArr, ZSPFPromBest,        &
     &        ZSDF, kappa, NSimul, HorPro, MinPro, SegPro, ULogCF)
         DO ISimul = 1, NSimul
            IF (ZSPFBestArr(ISimul) .GT. ZSPFArr(ISimul)) THEN
               ZSPFBestArr(ISimul) = ZSPFArr(ISimul)
               ITemp = Kit(ISimul, IMSPFHM)
               Kit(ISimul, IMSPFHM) = Kit(ISimul, ISPFA)
               Kit(ISimul, ISPFA) =  ITemp
               ITemp =  Kit(ISimul, IMSDFHM)
               Kit(ISimul, IMSDFHM) = Kit(ISimul, ISDFA)
               Kit(ISimul, ISDFA) =  ITemp
            ENDIF
         ENDDO

         IF (PDNumIte .GE. PDMaxIte) THEN
            PDConvrgVal = .TRUE.
         ENDIF
      ENDDO

      CALL Cerrar(UPlane)
      
      IF (FGrabaRES) THEN
         CALL GraDatPlaEmb3(PDNumIte - 1, ULog)
      ENDIF

      CALL PDMsjFin (ZSPFPromBest, ZSDF, PDNumIte, PDMaxIte,            &
     &     NSimul, FLog, ULog)
      CALL PDFin (PriProgDin, PDNumIte, ZSPFArr, ZSPFPromBest, ZSDF,    &
     &     kappa, NSimul, 6)
      CALL PDFin (PriProgDin, PDNumIte, ZSPFArr, ZSPFPromBest, ZSDF,    &
     &     kappa, NSimul, ULog)
!
      FLastPass = .TRUE.

      IF ( FLastPass ) THEN
         PDNumIte = PDNumIte - 1
         ArchContP(1:NEtapa, 1:NSimul) = 0
         PDSvFl = FSvLaPs
         MensajeProgreso =                                              &
     &        'progreso: ultima iter: '
         CALL FasePrim (IStat, PDNumIte, NEtapa, NCenEmb,               &
     &        NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,   &
     &        FScaleQs, ScaleObj, ScalePhi, ScaleVol,                   &
     &        FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,   &
     &        FConvLaja, ParLaja, ParLajaM,                             &
     &        FConvMaule, ParMaule,                                     &
     &        FRestRalco, ParRalco,                                     &
     &        FRestGnl, ParGnl,                                         &
     &        FRestReserva, ParRes,                                     &
     &        FBaterias, ParBaterias,                                   &
     &        PDNCol, PDNFila, ZSPFArr,                                 &
     &        PDSvFl, ErSvFl, PsFzFl,                                   &
     &        SimulInd,                                                 &
     &        PDLDAcFilaInd, PDLDAcColInd, EstocRHSP, EstocUBP,         &
     &        CenPMin, CenPMax, CenRen,                                 &
     &        CenManSInd, EmbVIni, NSimul,                              &
     &        EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,         &
     &        Kit,                                                      &
     &        FiltColInd, FiltEmbInd, FiltNCen,                         &
     &        FiltNTramo, FiltParam, FiltProm,                          &
     &        FFiltVar, FiltVarFilInd,                                  &
     &        RendColInd, RendFilaInd, RendEmbInd, RendNCen,            &
     &        RendCenInd, RendNTramo, RendParam, RendProm,              &
     &        PmaxColInd, PmaxEmbInd, PmaxNCen,                         &
     &        PmaxCenInd, PmaxNTramo, PmaxParam,                        &
     &        IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,             &
     &        Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,          &
     &        MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal,       &
     &        CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,         &
     &        CauConMauEta, CauRes105Eta,                               &
     &        MensajeProgreso,                                          &
     &        FLastPass,                                                &
     &        FOnePhi, FSeparaFCF, FDepHidEta,                          &
     &        FlagFilt, FlagRendProm, FRendBdrs, FInterfaz,             &
     &        FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &        ArchContP, ULog, ULogCF, lp, nthreads, kappa, Dim, 0)
         
      ENDIF


      WRITE(6, *)
      WRITE(6, '(A)')                                                &
     &     'progdin: Proceso finalizado exitosamente.'
      WRITE(ULog, *)
      WRITE(ULog, '(A)')                                             &
     &     'progdin: Proceso finalizado exitosamente.'

      CALL Cerrar(UWritePhi)
      CALL Cerrar(UWritePhi2)

      RETURN
      END
