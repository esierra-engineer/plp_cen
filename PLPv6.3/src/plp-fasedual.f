!****************************
!     Subrutina Algoritmo FaseDual
!****************************
      SUBROUTINE FaseDual(IStat, PDNumIte, NEtapa, NCenEmb,             &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,      &
     &     FScaleQs, ScaleObj, ScalePhi, ScaleVol,                      &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     PDNCol, PDNFila, PXNCol,                                     &
     &     ZSDF,                                                        &
     &     GradxPhi, LDPhiPrv, ApertInd, ApertInd2,                     &
     &     EstocFIndep, FDepHidEta,                                     &
     &     PDSvFl, ErSvFl, PsFzFl,                                      &
     &     PDLDAcNFila, PDLDAcNCol, PDLDAcFilaInd, PDLDAcColInd,        &
     &     EstocRHSP, EstocUBP, CenPMin, CenPMax, CenRen,               &
     &     CenManSInd, EmbVIni,                                         &
     &     NSimul, NApert, EstocNFila, EstocFilaInd,                    &
     &     EstocNCol, EstocColInd,                                      &
     &     Kit,                                                         &
     &     FiltColInd, FiltEmbInd, FiltNCen,                            &
     &     FiltNTramo, FiltParam, FiltProm,                             &
     &     FFiltVar, FiltVarFilInd,                                     &
     &     RendColInd, RendFilaInd, RendEmbInd, RendNCen,               &
     &     RendCenInd, RendNTramo, RendParam, RendProm,                 &
     &     PmaxColInd, PmaxEmbInd, PmaxNCen,                            &
     &     PmaxCenInd, PmaxNTramo, PmaxParam,                           &
     &     IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,                &
     &     Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,             &
     &     MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal,          &
     &     CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,            &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     MensajeProgreso,                                             &
     &     FOnePhi, FSeparaFCF, FSeparaLP,                              &
     &     FlagFilt, FlagRendProm, FRendBdrs, FInterfaz,                &
     &     SimulInd,                                                    &
     &     FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &     DualFactIter, OptiEPS, OptiMLD,                              &
     &     ArchContD, ArchContP, ULog, ULogCF, lp, nthreads, kappa, Dim,&
     &     UPlane)

#ifdef _OPENMP
      USE OMP_LIB
#endif

      USE PLP

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER UPlane
!     Convenios antiguos
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION CauDemRegAbaEta(Dim%Eta)
      DOUBLE PRECISION CauDemRegTucaEta(Dim%Eta)
      DOUBLE PRECISION CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION CauConMauEta(Dim%Eta)
      INTEGER MauleIPar(DimIMaule)
      INTEGER LajaIPar(DimILaja)
      TYPE(PAR_LAJAC) LajaCPar
      LOGICAL LajaLPar(DimLLaja)
      LOGICAL MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)
      TYPE(PAR_MAULEC) MauleCPar
      LOGICAL MesBal(Dim%Eta)

      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
!     Convenios antiguos


      INTEGER RendCenInd(Dim%EmbRend)
      INTEGER RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendNCen
      INTEGER RendNTramo(Dim%EmbRend)
      INTEGER RendEmbInd(Dim%EmbRend)
      DOUBLE PRECISION RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION RendProm(Dim%EmbRend, Dim%Eta + 1)

      INTEGER, INTENT(IN):: PmaxCenInd(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: PmaxNCen
      INTEGER, INTENT(IN):: PmaxNTramo(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxEmbInd(Dim%EmbPmax)
      DOUBLE PRECISION, INTENT(IN):: PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)
      
      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION EstocUBP(Dim%EstocCol, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION LDPhiPrv(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION EmbVIni(Dim%Emb, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      INTEGER DualFactIter
      LOGICAL FSeparaCFA, FOneFeasRay
      INTEGER FactMXC


      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION EtaDur(Dim%Eta)
      INTEGER Mes(Dim%Eta)
      INTEGER Year(Dim%Eta)
      DOUBLE PRECISION FactTiempo

      INTEGER FInterfaz
      LOGICAL FScaleQs
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleVol(Dim%Emb)
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)

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
      INTEGER nthreads
      LOGICAL FDepHidEta(Dim%Eta)
      INTEGER EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER EstocNCol
      INTEGER EstocNFila
      INTEGER FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER FiltNCen
      INTEGER FiltNTramo(Dim%EmbFilt)
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER IEtapa
      INTEGER ISimul
      INTEGER IStat
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)
      INTEGER NApert(Dim%Simul, Dim%Eta)
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NSimul
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      INTEGER PDLDAcNCol
      INTEGER PDLDAcNFila
      INTEGER PDNFila(Dim%Eta)
      INTEGER PDNCol(Dim%Eta)
      INTEGER PXNCol(Dim%Eta)
      INTEGER PDNumIte

      INTEGER ULog
      INTEGER ULogCF
      INTEGER Kit(Dim%Simul, Dim%Kit)
      LOGICAL EstocFIndep(Dim%Hid)
      INTEGER FlagFilt
      LOGICAL FlagRendProm
      LOGICAL FRendBdrs
      LOGICAL FOnePhi
      LOGICAL FSeparaFCF
      LOGICAL FSeparaLP
      LOGICAL PDSvFl
      LOGICAL ErSvFl
      LOGICAL PsFzFl
      INTEGER FactDBL
      CHARACTER*23 MensajeProgreso
      INTEGER NProgreso
      DOUBLE PRECISION kappa
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER ArchContD(Dim%Apert, Dim%Eta, Dim%Simul)
      INTEGER ArchContP(Dim%Eta, Dim%Simul)

      INTEGER IndEta1Imp
      INTEGER IndEta2Imp
      INTEGER IndIteImp
      INTEGER IndSimImp

!     codigo:
#ifndef _OPENMP
      IF (nthreads .GT. 1) THEN
         nthreads = 1
      ENDIF
#endif

      IF (FInterfaz .gt. 0) THEN
         NProgreso = NEtapa*NSimul
         CALL ReporteProgreso2(MensajeProgreso, 0, 0, NProgreso,        &
     &        Si, No)
      ENDIF
      ZSDF = 0.0d0
      DO IEtapa = NEtapa, 1, -1
         IF (IEtapa .NE. NEtapa) THEN
            CALL AgrResPD(IEtapa,                                       &
     &           EmbVini, ScaleVol,                                     &
     &           GradxPhi, LDPhiPrv,                                    & 
     &           ScalePhi, PDNumIte, PDNCol, PXNCol,                    &
     &           NSimul, NEtapa, NCenEmb,                               &
     &           PDLDAcNCol, PDLDAcColInd,                              &
     &           FOnePhi, FSeparaFCF, FSeparaLP, FDepHidEta,            &
     &           OptiEPS, OptiMLD, lp,                                  &
     &           FConvLaja, ParLaja, ParLajaM,                          &
     &           FConvMaule, ParMaule,                                  &
     &           FRestGnl, ParGnl,                                      &
     &           Dim, UPlane)
         ENDIF

         IStat = 1

!$OMP PARALLEL NUM_THREADS(nthreads) default(shared)  &
!$OMP& private(ISimul)  &
!$OMP& reduction(+:ZSDF)  &
!$OMP& reduction(MAX:IStat) &
!$OMP& reduction(MAX:kappa)
!$OMP DO SCHEDULE(RUNTIME)
         DO ISimul = 1, NSimul            
            CALL FaseDuali(IEtapa, ISimul,                              & 
     &           IStat, PDNumIte, NEtapa, NCenEmb,                      &
     &           NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,&
     &           FScaleQs, ScaleObj, ScalePhi, ScaleVol,                &
     &           FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,&
     &           FConvLaja, ParLaja, ParLajaM,                          &
     &           FConvMaule, ParMaule,                                  &
     &           FRestRalco, ParRalco,                                  &
     &           FRestGnl, ParGnl,                                      &
     &           FRestReserva, ParRes,                                  &
     &           FBaterias, ParBaterias,                                &
     &           PDNCol, PDNFila,                                       &
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
     &           FOnePhi, FSeparaFCF,                                   &
     &           FlagFilt, FlagRendProm, FRendBdrs,                     &
     &           SimulInd,                                              &
     &           FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &           DualFactIter, lp, kappa, 'f2',                         &
     &           ArchContD, ArchContP, ZSDF, ULog, ULogCF, Dim, UPlane)
         ENDDO
!$OMP END DO
!$OMP END PARALLEL

         IF (FInterfaz .gt. 0) THEN
            CALL ReporteProgreso2(MensajeProgreso, 0, NSimul, 0,        &
     &           No, No)
         ENDIF
      ENDDO
      ZSDF = ZSDF/DBLE(NSimul)
      IF (FInterfaz .gt. 0) THEN
         CALL ReporteProgreso2(MensajeProgreso, 72, 0, 0,                  &
     &        No, Si)
      ENDIF
      RETURN
      END


!****************************
!     Subrutina Algoritmo FaseDual
!****************************
      SUBROUTINE FaseDuali(IEtapa, ISimul,                              &
     &     IStat, PDNumIte, NEtapa, NCenEmb,                            &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,      &
     &     FScaleQs, ScaleObj, ScalePhi, ScaleVol,                      &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     PDNCol, PDNFila,                                             &
     &     GradxPhi, LDPhiPrv, ApertInd, ApertInd2,                     &
     &     EstocFIndep, FDepHidEta,                                     &
     &     PDSvFl, ErSvFl, PsFzFl,                                      &
     &     PDLDAcNFila, PDLDAcNCol, PDLDAcFilaInd, PDLDAcColInd,        &
     &     EstocRHSP, EstocUBP, CenPMin, CenPMax, CenRen,               &
     &     CenManSInd, EmbVIni,                                         &
     &     NSimul, NApert, EstocNFila, EstocFilaInd,                    &
     &     EstocNCol, EstocColInd,                                      &
     &     Kit,                                                         &
     &     FiltColInd, FiltEmbInd, FiltNCen,                            &
     &     FiltNTramo, FiltParam, FiltProm,                             &
     &     FFiltVar, FiltVarFilInd,                                     &
     &     RendColInd, RendFilaInd, RendEmbInd, RendNCen,               &
     &     RendCenInd, RendNTramo, RendParam, RendProm,                 &
     &     PmaxColInd, PmaxEmbInd, PmaxNCen,                            &
     &     PmaxCenInd, PmaxNTramo, PmaxParam,                           &
     &     IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,                &
     &     Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,             &
     &     MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal,          &
     &     CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,            &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     FOnePhi, FSeparaFCF,                                         &
     &     FlagFilt, FlagRendProm, FRendBdrs,                           &
     &     SimulInd,                                                    &
     &     FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &     DualFactIter, lp, kappa, ext,                                &
     &     ArchContD, ArchContP, ZSDF, ULog, ULogCF, Dim, UPlane)

      USE PLP
!     Constantes de la maquina.
      USE OSI
      INCLUDE 'machcons.fpp'

      TYPE(PAR_DIMS), INTENT(IN):: Dim

      INTEGER, INTENT(IN) :: UPlane
      CHARACTER*(*), INTENT(IN):: ext
      DOUBLE PRECISION, INTENT(IN):: Cau2Vol(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemRegAbaEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemRegTucaEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauConMauEta(Dim%Eta)
      INTEGER, INTENT(IN) :: MauleIPar(DimIMaule)
      INTEGER, INTENT(IN) :: LajaIPar(DimILaja)
      TYPE(PAR_LAJAC), INTENT(IN) :: LajaCPar
      LOGICAL, INTENT(IN):: LajaLPar(DimLLaja)
      TYPE(PAR_MAULEC), INTENT(IN) :: MauleCPar
      LOGICAL, INTENT(IN):: MesBal(Dim%Eta)
      INTEGER, INTENT(IN) :: RendCenInd(Dim%EmbRend)
      INTEGER, INTENT(IN) :: RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: RendNCen
      INTEGER, INTENT(IN) :: RendNTramo(Dim%EmbRend)
      INTEGER, INTENT(IN) :: RendEmbInd(Dim%EmbRend)
      DOUBLE PRECISION, INTENT(IN):: RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION, INTENT(IN):: RendProm(Dim%EmbRend, Dim%Eta + 1)

      INTEGER, INTENT(IN):: PmaxCenInd(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: PmaxNCen
      INTEGER, INTENT(IN):: PmaxNTramo(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxEmbInd(Dim%EmbPmax)
      DOUBLE PRECISION, INTENT(IN):: PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)
      
      
      INTEGER, INTENT(IN):: CenManSInd(Dim%Simul)
      DOUBLE PRECISION, INTENT(IN):: CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION, INTENT(IN):: CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: EstocUBP(Dim%EstocCol, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION, INTENT(IN):: FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION, INTENT(INOUT):: EmbVIni(Dim%Emb, Dim%Simul, Dim%Eta)

      DOUBLE PRECISION, INTENT(IN):: FactEPS
      DOUBLE PRECISION, INTENT(IN):: FactMLD
      LOGICAL, INTENT(IN):: FSeparaCFA, FOneFeasRay
      INTEGER, INTENT(IN) :: DualFactIter
      INTEGER, INTENT(IN) :: FactMXC
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      INTEGER, INTENT(IN) :: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: EtaDur(Dim%Eta)
      INTEGER, INTENT(IN) :: Mes(Dim%Eta)
      INTEGER, INTENT(IN) :: Year(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: FactTiempo
      INTEGER, INTENT(IN) :: SimulInd(Dim%Simul, Dim%Eta)

      LOGICAL, INTENT(IN):: FScaleQs
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
      DOUBLE PRECISION, INTENT(IN):: ScalePhi
      DOUBLE PRECISION, INTENT(IN):: ScaleVol(Dim%Emb)
      LOGICAL, INTENT(IN):: FVertReb
      DOUBLE PRECISION, INTENT(IN):: EmbVReb(Dim%EmbVReb)
      INTEGER, INTENT(IN) :: NEmbVReb
      INTEGER, INTENT(IN) :: EmbVRebInd(Dim%EmbVReb)
      INTEGER, INTENT(IN) :: EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)
      INTEGER, INTENT(IN) :: FConvLaja
      TYPE(PAR_LAJA), INTENT(INOUT) :: ParLaja
      TYPE(PAR_LAJAM), INTENT(INOUT) :: ParLajaM
      INTEGER, INTENT(IN) :: FConvMaule
      TYPE(PAR_MAULE), INTENT(INOUT) :: ParMaule
      LOGICAL, INTENT(IN) :: FRestRalco
      TYPE(PAR_RALCO), INTENT(IN) :: ParRalco
      LOGICAL, INTENT(IN) :: FRestGnl
      TYPE(PAR_GNL), INTENT(INOUT) :: ParGnl
      LOGICAL,INTENT(IN) :: FRestReserva
      TYPE(PAR_GNL),INTENT(IN) :: ParRes
      LOGICAL,INTENT(IN) :: FBaterias
      TYPE(PAR_BATERIAS),INTENT(IN) :: ParBaterias

      LOGICAL, INTENT(IN):: FFiltVar
      INTEGER, INTENT(IN) :: FiltVarFilInd(Dim%EmbFilt, Dim%Eta)

      INTEGER, INTENT(IN) :: ApertInd(Dim%Apert, Dim%Simul, Dim%Eta)
      INTEGER, INTENT(IN) :: ApertInd2(Dim%Apert, Dim%Eta)
      LOGICAL, INTENT(IN) :: FDepHidEta(Dim%Eta)
      INTEGER, INTENT(IN) :: EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: EstocNCol
      INTEGER, INTENT(IN) :: EstocNFila
      INTEGER, INTENT(IN) :: FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: FiltNCen
      INTEGER, INTENT(IN) :: FiltNTramo(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: FiltEmbInd(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul
      INTEGER, INTENT(IN) :: NApert(Dim%Simul, Dim%Eta)
      INTEGER, INTENT(IN) :: NEtapa
      INTEGER, INTENT(IN) :: NCenEmb
      INTEGER, INTENT(IN) :: NSimul
      INTEGER, INTENT(IN) :: PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER, INTENT(IN) :: PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      INTEGER, INTENT(IN) :: PDLDAcNCol
      INTEGER, INTENT(IN) :: PDLDAcNFila
      INTEGER, INTENT(IN) :: PDNCol(Dim%Eta)
      INTEGER, INTENT(IN) :: PDNFila(Dim%Eta)
      INTEGER, INTENT(IN) :: PDNumIte
      INTEGER, INTENT(IN) :: ULog
      INTEGER, INTENT(IN) :: ULogCF
      LOGICAL, INTENT(IN) :: EstocFIndep(Dim%Hid)
      INTEGER, INTENT(IN) :: FlagFilt
      LOGICAL, INTENT(IN) :: FlagRendProm
      LOGICAL, INTENT(IN) :: FRendBdrs
      LOGICAL, INTENT(IN) :: FOnePhi
      LOGICAL, INTENT(IN) :: FSeparaFCF
      LOGICAL, INTENT(IN) :: PDSvFl
      LOGICAL, INTENT(IN) :: ErSvFl
      LOGICAL, INTENT(IN) :: PsFzFl
      INTEGER, INTENT(IN) :: IndEta1Imp
      INTEGER, INTENT(IN) :: IndEta2Imp
      INTEGER, INTENT(IN) :: IndIteImp
      INTEGER, INTENT(IN) :: IndSimImp
      INTEGER, INTENT(IN) :: FactDBL

      INTEGER, INTENT(IN) :: Kit(Dim%Simul, Dim%Kit)

!     out

      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION, INTENT(OUT):: LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      LOGICAL, INTENT(OUT):: MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION, INTENT(OUT):: MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)

      INTEGER, INTENT(OUT) :: IStat

      DOUBLE PRECISION, INTENT(INOUT):: kappa
      INTEGER, INTENT(INOUT) :: ArchContD(Dim%Apert, Dim%Eta, Dim%Simul)
      INTEGER, INTENT(INOUT) :: ArchContP(Dim%Eta, Dim%Simul)

      DOUBLE PRECISION, INTENT(OUT):: GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION, INTENT(OUT):: LDPhiPrv(Dim%Simul, Dim%Eta)

      DOUBLE PRECISION, INTENT(INOUT) :: ZSDF


!     local

      DOUBLE PRECISION CenReni(Dim%Cen)

      DOUBLE PRECISION PromedioZ
      DOUBLE PRECISION Z
      DOUBLE PRECISION VolIni(PDLDAcNCol)
      DOUBLE PRECISION PromedioPi(PDLDAcNFila)

      DOUBLE PRECISION kappai

      INTEGER(C_SIZE_T) lpi

      INTEGER IApert
      INTEGER ICiclo

      INTEGER IClaseFila(EstocNFila)
      INTEGER IClaseCol(EstocNCol)

      INTEGER IEstocCol
      INTEGER IEstocFila

      INTEGER IEta
      INTEGER IEtaDest
      INTEGER IEtap
      INTEGER ifact
      INTEGER ifactn
      LOGICAL Infactible
      LOGICAL FlagFiltProm
      DOUBLE PRECISION ZSPFAdd
      DOUBLE PRECISION FiltVals(FiltNCen)
      DOUBLE PRECISION ZSPFArr(NSimul)

      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION AflMel
      DOUBLE PRECISION AflRuc

      INTEGER NumEmb
      INTEGER NEtapai
      CHARACTER*80 filename

      INTEGER NAperti


!
      NumEmb = NCenEmb


      ZSPFArr(1:NSimul) = 0.d0

      IEta = IEtapa
      DO WHILE (.TRUE.) 
         lpi = lp(ISimul, IEta)
         VolIni(1:NumEmb) = EmbVIni(1:NumEmb, ISimul, IEta)


         IF (.NOT. FFiltVar) THEN
            FlagFiltProm = FlagFilt .EQ. FILT_PROM
            CALL Filtrac(IEta, NBloque(IEta),                           &
     &           FiltNCen, FlagFiltProm,                                &
     &           FiltProm, FiltColInd, FiltEmbInd, FiltNTramo,          &
     &           FiltParam, VolIni,                                     &
     &           lpi, Dim)
         ELSE
            CALL Filtracv(IEta,                                         &
     &           FiltNCen, FlagFilt,                                    &
     &           FiltProm, FiltVarFilInd,                               &
     &           FiltEmbInd, FiltNTramo,                                &
     &           FiltParam, VolIni, FiltVals, PDLDAcColInd, ScaleVol,   &
     &           lpi, Dim)
         ENDIF

         CALL Rendim(IEta, NBloque(IEta), RendNCen, FlagRendProm,       &
     &        RendProm, RendColInd, RendFilaInd, RendEmbInd, RendCenInd,&
     &        RendNTramo, RendParam, FRendBdrs,                         &
     &        PmaxNCen, PmaxColInd, PmaxEmbInd,                         &
     &        PmaxCenInd, PmaxNTramo, PmaxParam,                        &
     &        BloInd(1, IEta),                                          &
     &        CenRen,                                                   &
     &        CenPMin(1, 1, CenManSInd(ISimul)),                        &
     &        CenPMax(1, 1, CenManSInd(ISimul)),                        &
     &        VolIni, CenReni,                                          &
     &        FRestReserva, ParRes,                                     &
     &        lpi, Dim)
         CALL Laja1Dual(IEta, NBloque,                                  &
     &        LajaCPar, LajaLPar, LajaRPar,                             &
     &        ISimul, 1D9,                                              &
     &        lpi, Dim)

         ifact = 0
         ifactn = 0
         PromedioZ = 0.0d0
         PromedioPi(1:PDLDAcNFila) = 0.0d0
         NAperti = NApert(ISimul, IEta)
         DO IApert = 1, NAperti
!     Las cotas se deben reasignar en cada apertura, pues los caudales d
!     aperturas se suman a los volumenes embalsados iniciales (gomen).
            CALL FijaSimu(IEta, ScaleVol,                               &
     &           VolIni,                                                &
     &           FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,&
     &           NumEmb,                                                & 
     &           PDLDAcFilaInd,                                         &
     &           lpi, Dim)                                     
            CALL FijaVar(IEta,                                          &
     &           ScaleObj, ScalePhi,                                    &
     &           PDNCol,                                                &
     &           ISimul, NSimul, PDNumIte + 1, NEtapa,                  &
     &           FOnePhi, FSeparaFCF, FDepHidEta,                       &
     &           lpi)                                     
            DO IEstocFila = 1, EstocNFila                            
               IF ((FDepHidEta(IEta)) .AND.                             &
     &              EstocFIndep(IEstocFila)) THEN
                  IClaseFila(IEstocFila) = ApertInd2(IApert, IEta)
               ELSE
                  IClaseFila(IEstocFila) = ApertInd(IApert, ISimul, IEta)
               ENDIF
            ENDDO

            DO IEstocCol = 1, EstocNCol
               IF ((FDepHidEta(IEta)) .AND.                &
     &              EstocFIndep(EstocNFila + IEstocCol)) THEN
                  IClaseCol(IEstocCol) = ApertInd2(IApert, IEta)
               ELSE
                  IClaseCol(IEstocCol) = ApertInd(IApert, ISimul, IEta)
               ENDIF
            ENDDO

            CALL FijaMues(IEta, NBloque(IEta),                          &
     &           CenPMax(1, 1, CenManSInd(ISimul)), CenReni,            &
     &           BloInd(1, IEta), BloDur, FactTiempo, FScaleQs,         &
     &           EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,      &
     &           EstocRHSP, EstocUBP,                                   &
     &           IClaseFila, IClaseCol,                                 &
     &           FBaterias, ParBaterias,                                &
     &           lpi, Dim)

!------------------------------------------------------------------------
           IF (MauleIPar(IIUsoConvMaule) .NE. 0) THEN
                AflMel = QAfluEta(IEta, NBloque, BloInd, BloDur,        &
     &            EstocRHSP, IClaseFila, MauleIPar(IIAflPehuenche), Dim)

                CALL Maule1Dual(IEta, NBloque,                          &
     &              Cau2Vol, MauleIPar, MauleCPar, MauleRPar,           &
     &              ISimul, AflMel,                                     &
     &              lpi, Dim)
           ENDIF

 	 
!-----------------------------------------------------------------------
           IF (LajaLPar(IUsoConvLaja)) THEN
               AflRuc = QAfluEta(IEta, NBloque, BloInd, BloDur,         &
     &            EstocRHSP, IClaseFila, LajaIPar(IIAflRucue), Dim)
               CALL Laja1Dual(IEta, NBloque,                            &
                   LajaCPar, LajaLPar, LajaRPar,                        &
                   ISimul, AflRuc,                                      &
                   lpi, Dim)
           ENDIF
!-----------------------------------------------------------------------

            IF (FConvLaja .EQ. 1) THEN
               CALL FijaLaja(IEta,                                      &
     &              NBloque, BloInd, BloDur, EtaDur, Mes, FactTiempo,   &     
     &              EstocRHSP, IClaseFila,                              &
     &              ParLaja, VolIni, NumEmb,                            &
     &              ISimul,                                             &
     &              lpi, Dim)
            ENDIF
            IF (FConvLaja .EQ. 2) THEN
               CALL FijaLajaM(IEta,                                     &
     &              NBloque, BloInd, BloDur, EtaDur, Mes, FactTiempo,   &     
     &              EstocRHSP, IClaseFila,                              &
     &              ParLajaM,ParRes, VolIni, NumEmb, FiltVals,                 &
     &              ISimul,FRestReserva,                                             &
     &              lpi, Dim)
            ENDIF
            IF (FConvMaule .NE. 0) THEN
               CALL FijaMaule(IEta,                                     &
     &              NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,   &     
     &              EstocRHSP, IClaseFila,                              &
     &              ParMaule, VolIni, NumEmb, FiltVals,                 &
     &              ISimul,                                             &
     &              lpi, Dim)
            ENDIF

            IF (FRestRalco) THEN
               CALL FijaRalco(IEta,                                     &
     &              VolIni, NumEmb, ParRalco, lpi)
            ENDIF

            IF (FRestGnl) THEN
               CALL FijaGnl(IEta, ISimul, VolIni, NumEmb, ParGnl, lpi)
            ENDIF


            ICiclo = ArchContD(IApert, IEta, ISimul)

            IF (PDSvFl) THEN
               CALL Salvai(IEta, PDNumIte + 1, ISimul,               &
     &              IApert, 0, ext//'D', Si, ICiclo, ULog, lpi,      & 
     &              .TRUE.)
            ENDIF

            kappai = 0.0d0
            Infactible = .FALSE.
            CALL Resuelve(IStat, lpi, kappai, Dim)
            ArchContD(IApert,IEta, ISimul) =                            & 
     &           ArchContD(IApert, IEta, ISimul) + 1
            IF (IStat .EQ. OSI_STAT_OPTIMAL) THEN
               CALL TrasDual(ScaleObj,                                  &
     &              PDLDAcFilaInd(1, IEta), PDLDAcNFila,                &
     &              Z, PromedioPi,                                      &
     &              lpi)
               PromedioZ = PromedioZ + Z

               IF (kappai .GT. kappa) THEN
                  kappa = kappai
               ENDIF
            ELSEIF (IStat .EQ. OSI_STAT_INFEASIBLE) THEN
               Infactible = .TRUE.
               IF (FactDBL .NE. 0) THEN               
!                 Log cortes de factibilidad
                  WRITE(ULogCF,'(A, I3, A, I3, A, I3)')                 &  
     &                 'Apert Infac Simul=',                            &
     &                 ISimul, ' Etapa=', IEta, ' IAprt=', IApert
               ENDIF

               IF (ICiclo + 1 .GT. FactMXC) THEN
                  WRITE(6, '(A, I3, A, I3)')                            &
     &                 'Problema Infactible Ciclo=',                    &
     &                 ICiclo + 1, ' supera limite ', FactMXC            
                  WRITE(ULog, '(A, I3, A, I3)')                         &
     &                 'Problema Infactible Ciclo=',                    &
     &                 ICiclo + 1, ' supera limite ', FactMXC                     
!                 Log cortes de factibilidad
                  WRITE(ULogCF, '(A, I3, A, I3)')                       &
     &                 'Problema Infactible Ciclo=',                    &
     &                 ICiclo + 1, ' supera limite ', FactMXC            
                  Infactible = .TRUE.
               ELSE IF (IEta .GT. 1) THEN
!                 Tratamos de agregar un corte de infactibilidad 
                  IF (FactDBL .GT. 1) THEN
                     CALL Salvai(IEta, PDNumIte + 1, ISimul, IApert,    &
     &                    0, ext//'O', Si, ICiclo, ULog, lpi, .TRUE.)

                     CALL name4salva(filename, iapert, ietapa, isimul,  &
     &                    PDNumIte+1, PDNumIte+1, ext//'L', Si, iciclo)
                  ENDIF
                  IEtaDest = IEta - 1
                  CALL AgrFact( &
     &                 FactEPS, FactMLD, FactDBL, FSeparaCFA, FOneFeasRay, &
     &                 PDNumIte, NSimul, ISimul, IEta, IEtaDest,        &
     &                 NCenEmb,                                         &
     &                 FConvLaja, Parlaja, ParLajaM,                    &
     &                 FConvMaule, Parmaule,                            &
     &                 PDLDAcFilaInd,                                   &
     &                 PDLDAcColInd,                                    &
     &                 lp, IStat, filename, ULogCF, Dim, UPlane)
                  IF (IStat .EQ. 0) THEN
                     ifact = ifact + 1                     
                     IF (FactDBL .NE. 0) THEN                                       
!                       Log cortes de factibilidad
                        WRITE(ULogCF, '(A, I5, A, I3, A, I3, A, I3)')      &
     &                       'Se agrego corte de factibilidad ICiclo=',    & 
     &                       ICiclo, ' EtaDest=', IEtaDest, ' Simul=',     &
     &                       ISimul, ' Apert=', IApert 
                     ENDIF
                     Infactible = .FALSE.
                  ENDIF

                  IF (IStat .EQ. -1) THEN
                     ifactn = ifactn + 1                     
                     IF (FactDBL .NE. 0) THEN
!                       Log cortes de factibilidad
                        WRITE(ULogCF, &
     &                       '(A, I5, A, I3, A, I3, A, I3, A, I3)')        &
     &                       'Infactible superada con holguras  ICiclo=',  & 
     &                       ICiclo, ' EtaDest=', IEtaDest, ' Simul=',     &
     &                       ISimul, ' Apert=', IApert, &
     &                       ' ifactn=', ifactn
                     ENDIF
                     Infactible = .FALSE.
                     IStat = 0
                  ENDIF
               ENDIF
            ELSE            
               IF (ErSvFl) THEN
                  CALL Salvai(IEta, PDNumIte + 1, ISimul,               &
     &                 IApert, 0, ext//'E', Si, ICiclo, ULog, lpi,      &
     &                 .TRUE.)
               ENDIF
               WRITE(ULog, '(A, I4, A, I3, A, I3, A, I3, A)')           &
     &              'fasedual: Error en etapa/simul ',                  &
     &              IEta, '/', ISimul, '/', IApert,  &
     &              ' en la fase dual con flag ', IStat, '.'
               IF (PsFzFl) THEN
!     Es posible que el error no sea tan grave...
                  IStat = OSI_STAT_OPTIMAL
               ELSE
                  STOP 1
               ENDIF
            ENDIF
            IF (Infactible) THEN
               IF (ErSvFl) THEN
                  CALL Salvai(IEta, PDNumIte + 1, ISimul, IApert,       &
     &                 0, ext//'I', Si, ICiclo, ULog, lpi, .TRUE.)
               ENDIF
               WRITE(ULog, '(A, I4, A, I3, A, I3, A, I3, A)')           &
     &              'fasedual: Error en etapa/simul ',                  &
     &              IEta, '/', ISimul, '/', IApert,  &
     &              ' en la fase dual con flag ', IStat, '.'
               IF (PsFzFl) THEN
!     Es posible que el error no sea tan grave...
                  IStat = OSI_STAT_OPTIMAL
               ELSE
                  STOP 1
               ENDIF
            ENDIF            
         ENDDO

         IF (ifact .GT. 0) THEN
            IF (FactDBL .NE. 0) THEN                                       
!              Log cortes de factibilidad
               WRITE(ULogCF, '(A, I3, A)')                              &
     &              'Se resuelven infactibilidades ifact=', ifact, &
     &              ' en fase forward' 
            ENDIF

!     Se agrego algun corte de factibilidad. Se requiere entonces hacer 
!     fast forward desde la etapa anterior hasta el final, y luego devolverse  
!     en modo backward hasta esta misma etapa.
            IF (PDNumIte .lt. DualFactIter) THEN
               NEtapai = IEta

               WRITE(ULogCF, '(A, I3)') 'Fase dual reducida iter=', PDNumIte
            ELSE
               NEtapai = NEtapa 
            ENDIF
            DO IEtap = IEta - 1, NEtapai
               CALL FasePrimi(IEtap, ISimul, IStat, PDNumIte,           &
     &              NEtapa, NCenEmb,                                    &
     &              NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,   &
     &              FScaleQs, ScaleObj, ScalePhi, ScaleVol,             &
     &              FVertReb, NEmbVReb, EmbVRebInd,                     &
     &              EmbVRebFilInd, EmbVReb,                             &
     &              FConvLaja, ParLaja, ParLajaM,                       &
     &              FConvMaule, ParMaule,                               &
     &              FRestRalco, ParRalco,                               &
     &              FRestGnl, ParGnl,                                   &
     &              FRestReserva, ParRes,                               &
     &              FBaterias, ParBaterias,                             &
     &              PDNCol, PDNFila, ZSPFAdd,                           &
     &              PDSvFl, ErSvFl, PsFzFl,                             &
     &              SimulInd,                                           &
     &              PDLDAcFilaInd, PDLDAcColInd, EstocRHSP, EstocUBP,   &
     &              CenPMin, CenPMax, CenRen,                           &
     &              CenManSInd, EmbVIni, NSimul,                        &
     &              EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,   &
     &              Kit,                                                &
     &              FiltColInd, FiltEmbInd, FiltNCen,                   &
     &              FiltNTramo, FiltParam, FiltProm,                    &
     &              FFiltVar, FiltVarFilInd,                            &
     &              RendColInd, RendFilaInd, RendEmbInd, RendNCen,      &
     &              RendCenInd, RendNTramo, RendParam, RendProm,        &
     &              PmaxColInd, PmaxEmbInd, PmaxNCen,                   &
     &              PmaxCenInd, PmaxNTramo, PmaxParam,                  &
     &              IndSimImp, IndEta1Imp, IndEta2Imp, IndIteImp,       &
     &              Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,    &
     &              MauleIPar, MauleCPar, MauleLPar, MauleRPar, MesBal, &
     &              CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,   &
     &              CauConMauEta, CauRes105Eta,                         &
     &              .FALSE.,                                            &
     &              FOnePhi, FSeparaFCF, FDepHidEta,                    &
     &              FlagFilt, FlagRendProm, FRendBdrs,                  &
     &              FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay,     &
     &              ZSPFArr(ISimul),                                    &
     &              lp, kappa, 'F1', ArchContP, ULog, ULogCF, Dim, UPlane)
               
               IF (IStat .NE. OSI_STAT_OPTIMAL) THEN
                  WRITE(ULog, '(A, A, I3, A, I3, A, I3, A)')            &
     &                 'fasedual: No es posible avanzar forward  desde',&
     &                 ' eta=', IEta, ' sim=', ISimul,                  &
     &                 ' en la fase dual con flag ', IStat, '.'
                  WRITE(ULogCF, '(A, A, I3, A, I3, A, I3, A)')          &
     &                 'fasedual: No es posible avanzar forward  desde',&
     &                 ' eta=', IEta, ' sim=', ISimul,                  &
     &                 ' en la fase dual con flag ', IStat, '.'
                  WRITE(6, '(A, A, I3, A, I3, A, I3, A)')               &
     &                 'fasedual: No es posible avanzar forward desde ',&
     &                 ' eta=', IEta, ' sim=', ISimul,                  &
     &                 ' en la fase dual con flag ', IStat, '.'
                  STOP 1
               ENDIF
            ENDDO

            IEta = NEtapai
            CYCLE               
         ENDIF
                          
         IF (IEta .GT. IEtapa) THEN
            IEta = IEta - 1
            CYCLE
         ENDIF            

!     
!     Termino del while loop
!
         IF (IEta .GT. 1) THEN
            NAperti = NAperti - ifact - ifactn
            IF (NAperti .ge. 1) THEN
            CALL EsperCnd(PromedioZ, PromedioPi, IEta,                  &
     &           GradxPhi, LDPhiPrv,                                    & 
     &              ISimul, NAperti,                                    &
     &           PDLDAcNFila,                                           &
     &           PDLDAcNCol,                                            &
     &           ULog, Dim)
            ELSE
               WRITE(ULog, '(A, A, I3, A, I3, A, I3, A)')               &
     &              'fasedual: No es posible crear corte optimo, ',     &
     &              ' eta=', IEta, ' sim=', ISimul,                     &
     &              ' en la fase dual con flag ', IStat, '.'
               
            ENDIF
         ENDIF

         IF (IEta .EQ. 1) THEN
            ZSDF = ZSDF + PromedioZ
         ENDIF

         EXIT
      ENDDO

      RETURN
      END

