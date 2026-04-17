!****************************
!     Subrutina Algoritmo FasePrim
!****************************
      SUBROUTINE FasePrim(IStat, PDNumIte, NEtapa, NCenEmb,             &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,      &
     &     FScaleQs, ScaleObj, ScalePhi, ScaleVol,                      &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     PDNCol, PDNFila, ZSPFArr,                                    &
     &     PDSvFl, ErSvFl, PsFzFl,                                      &
     &     SimulInd,                                                    &
     &     PDLDAcFilaInd, PDLDAcColInd, EstocRHSP, EstocUBP,            &
     &     CenPMin, CenPMax, CenRen,                                    &
     &     CenManSInd, EmbVIni, NSimul,                                 &
     &     EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,            &
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
     &     FLastPass,                                                   &
     &     FOnePhi, FSeparaFCF, FDepHidEta,                             &
     &     FlagFilt, FlagRendProm, FRendBdrs, FInterfaz,                &
     &     FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &     ArchContP, ULog, ULogCF, lp, nthreads, kappa, Dim, UPlane)

!     Archivo comun a todas las rutinas.
#ifdef _OPENMP
      USE OMP_LIB
#endif
      USE PLP
!     Constantes de la maquina.
      INCLUDE 'machcons.fpp'
!
!
      INTEGER UPlane
      TYPE(PAR_DIMS), INTENT(IN):: Dim

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


!     filtrendbeg
      INTEGER RendCenInd(Dim%EmbRend)
      INTEGER RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendNCen
      INTEGER RendNTramo(Dim%EmbRend)
      INTEGER RendEmbInd(Dim%EmbRend)
      DOUBLE PRECISION RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION RendProm(Dim%EmbRend, Dim%Eta + 1)

      INTEGER PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER PmaxNCen
      INTEGER PmaxNTramo(Dim%EmbPmax)
      INTEGER PmaxEmbInd(Dim%EmbPmax)
      INTEGER PmaxCenInd(Dim%EmbPmax)
      DOUBLE PRECISION PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)


      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      LOGICAL FSeparaCFA, FOneFeasRay
      INTEGER FactMXC


!     filtrend
!     variables estables:
      INTEGER FInterfaz
      LOGICAL FDepHidEta(Dim%Eta)
      INTEGER nthreads

      INTEGER EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER EstocNCol
      INTEGER EstocNFila
      INTEGER FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER FiltNCen
      INTEGER FiltNTramo(Dim%EmbFilt)
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER IContador
      INTEGER IEtapa
      INTEGER IndEta1Imp
      INTEGER IndEta2Imp
      INTEGER IndIteImp
      INTEGER IndSimImp
      INTEGER ISimul
      INTEGER IStat
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)

      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NSimul
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      INTEGER PDNCol(Dim%Eta)
      INTEGER PDNFila(Dim%Eta)
      INTEGER PDNumIte
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER ULog
      INTEGER ULogCF
      INTEGER Kit(Dim%Simul, Dim%Kit)
      LOGICAL FLastPass
      INTEGER FlagFilt
      LOGICAL FlagRendProm
      LOGICAL FRendBdrs
      LOGICAL FOnePhi
      LOGICAL FSeparaFCF
      LOGICAL PDSvFl
      LOGICAL ErSvFl
      LOGICAL PsFzFl
      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION EmbVIni(Dim%Emb, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION EstocUBP(Dim%EstocCol, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION ZSPFArr(Dim%Simul)
      DOUBLE PRECISION ZSPFAdd

      INTEGER FactDBL

      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION EtaDur(Dim%Eta)
      DOUBLE PRECISION FactTiempo
      INTEGER Mes(Dim%Eta)
      INTEGER Year(Dim%Eta)

      LOGICAL FScaleQs
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleVol(Dim%Emb)
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)
      INTEGER EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)


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
      TYPE(PAR_RESERVA) ParRes
      LOGICAL FBaterias
      TYPE(PAR_BATERIAS) ParBaterias


      LOGICAL FFiltVar
      INTEGER FiltVarFilInd(Dim%EmbFilt, Dim%Eta)


      CHARACTER*23 MensajeProgreso
      INTEGER NProgreso

      INTEGER ArchContP(Dim%Eta, Dim%Simul)
      DOUBLE PRECISION kappa
      DOUBLE PRECISION kappae

!     codigo:

#ifndef _OPENMP
      IF (nthreads .GT. 1) THEN
         nthreads = 1
      ENDIF
#endif

      IF (FInterfaz .gt. 0) THEN
         NProgreso = NEtapa*NSimul
         CALL ReporteProgreso2(MensajeProgreso, 0, 0, NProgreso, Si, No)
      ENDIF
      IContador = 0
      ZSPFArr(1:NSimul) = 0.d0
      IStat = 1

!$OMP PARALLEL NUM_THREADS(nthreads) &
!$OMP& default(shared) &
!$OMP& private(ISimul, IEtapa, kappae) &
!$OMP& reduction(MAX:IStat) &
!$OMP& reduction(MAX:kappa)
!$OMP DO SCHEDULE(RUNTIME)
      DO ISimul = 1, NSimul
         kappae = kappa
         DO IEtapa = 1, NEtapa
            CALL FasePrimi(IEtapa, ISimul, IStat, PDNumIte,             &
     &           NEtapa, NCenEmb,                                       &
     &           NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,&
     &           FScaleQs, ScaleObj, ScalePhi, ScaleVol,                &
     &           FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,&
     &           FConvLaja, ParLaja, ParLajaM,                          &
     &           FConvMaule, ParMaule,                                  &
     &           FRestRalco, ParRalco,                                  &
     &           FRestGnl, ParGnl,                                      &
     &           FRestReserva, ParRes,                                  &
     &           FBaterias, ParBaterias,                                &
     &           PDNCol, PDNFila, ZSPFAdd,                              &
     &           PDSvFl, ErSvFl, PsFzFl,                                &
     &           SimulInd,                                              &
     &           PDLDAcFilaInd, PDLDAcColInd, EstocRHSP, EstocUBP,      &
     &           CenPMin, CenPMax, CenRen,                              &
     &           CenManSInd, EmbVIni, NSimul,                           &
     &           EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,      &
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
     &           FLastPass,                                             &
     &           FOnePhi, FSeparaFCF, FDepHidEta,                       &
     &           FlagFilt, FlagRendProm, FRendBdrs,                     &
     &           FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay,        &
     &           ZSPFArr(ISimul),                                       &
     &           lp, kappae, 'f1', ArchContP, ULog, ULogCF, Dim, UPlane)
            kappa = kappae
            IF (ISimul .eq. 1) THEN
               IF (FInterfaz .gt. 0) THEN
                  CALL ReporteProgreso2(MensajeProgreso, 0, NSimul, 0, No, No)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

      IF (FInterfaz .gt. 0) THEN
         CALL ReporteProgreso2(MensajeProgreso, 72, 0, 0,                  &
     &        No, Si)
      ENDIF


      RETURN
      END


!****************************
!     Subrutina Algoritmo FasePrim
!****************************
      SUBROUTINE FasePrimi(IEtapa, ISimul, IStat, PDNumIte,             &
     &     NEtapa, NCenEmb,                                             &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,      &
     &     FScaleQs, ScaleObj, ScalePhi, ScaleVol,                      &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     PDNCol, PDNFila, ZSPFAdd,                                    &
     &     PDSvFl, ErSvFl, PsFzFl,                                      &
     &     SimulInd,                                                    &
     &     PDLDAcFilaInd, PDLDAcColInd, EstocRHSP, EstocUBP,            &
     &     CenPMin, CenPMax, CenRen,                                    &
     &     CenManSInd, EmbVIni, NSimul,                                 &
     &     EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,            &
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
     &     FLastPass,                                                   &
     &     FOnePhi, FSeparaFCF, FDepHidEta,                             &
     &     FlagFilt, FlagRendProm, FRendBdrs,                           &
     &     FactEPS, FactMLD, FactDBL, FactMXC, FSeparaCFA, FOneFeasRay, &
     &     ZSPFArr,                                                     &
     &     lp, kappa, ext, ArchContP, ULog, ULogCF, Dim, UPlane)
!     Archivo comun a todas las rutinas.
      USE PLP
      USE OSI
!     Constantes de la maquina.
      INCLUDE 'machcons.fpp'
!
      TYPE(PAR_DIMS), INTENT(IN):: Dim
!
      INTEGER, INTENT(IN):: UPlane
      CHARACTER*(*), INTENT(IN):: ext

!     Convenios antiguos
      DOUBLE PRECISION, INTENT(IN):: Cau2Vol(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemRegAbaEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemRegTucaEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauConMauEta(Dim%Eta)
      INTEGER, INTENT(IN):: MauleIPar(DimIMaule)
      INTEGER, INTENT(IN):: LajaIPar(DimILaja)
      TYPE(PAR_LAJAC), INTENT(IN):: LajaCPar
      LOGICAL, INTENT(IN):: LajaLPar(DimLLaja)
      TYPE(PAR_MAULEC), INTENT(IN):: MauleCPar
      LOGICAL, INTENT(IN):: MesBal(Dim%Eta)

      INTEGER, INTENT(IN):: RendCenInd(Dim%EmbRend)
      INTEGER, INTENT(IN):: RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: RendNCen
      INTEGER, INTENT(IN):: RendNTramo(Dim%EmbRend)
      INTEGER, INTENT(IN):: RendEmbInd(Dim%EmbRend)
      DOUBLE PRECISION, INTENT(IN):: RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION, INTENT(IN):: RendProm(Dim%EmbRend, Dim%Eta + 1)

      INTEGER, INTENT(IN):: PmaxCenInd(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: PmaxNCen
      INTEGER, INTENT(IN):: PmaxNTramo(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxEmbInd(Dim%EmbPmax)
      DOUBLE PRECISION, INTENT(IN):: PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)


      DOUBLE PRECISION, INTENT(IN):: FactEPS
      DOUBLE PRECISION, INTENT(IN):: FactMLD
      LOGICAL, INTENT(IN):: FSeparaCFA, FOneFeasRay
      INTEGER, INTENT(IN):: FactMXC

      LOGICAL, INTENT(IN):: FDepHidEta(Dim%Eta)
      INTEGER, INTENT(IN):: EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: EstocNCol
      INTEGER, INTENT(IN):: EstocNFila
      INTEGER, INTENT(IN):: FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: FiltNCen
      INTEGER, INTENT(IN):: FiltNTramo(Dim%EmbFilt)
      INTEGER, INTENT(IN):: FiltEmbInd(Dim%EmbFilt)
      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: IndEta1Imp
      INTEGER, INTENT(IN):: IndEta2Imp
      INTEGER, INTENT(IN):: IndIteImp
      INTEGER, INTENT(IN):: IndSimImp
      INTEGER, INTENT(IN):: ISimul

      INTEGER, INTENT(IN):: NEtapa
      INTEGER, INTENT(IN):: NCenEmb
      INTEGER, INTENT(IN):: NSimul
      INTEGER, INTENT(IN):: PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER, INTENT(IN):: PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      INTEGER, INTENT(IN):: PDNCol(Dim%Eta)
      INTEGER, INTENT(IN):: PDNFila(Dim%Eta)
      INTEGER, INTENT(IN):: PDNumIte
      INTEGER, INTENT(IN):: SimulInd(Dim%Simul, Dim%Eta)
      INTEGER, INTENT(IN):: ULog
      INTEGER, INTENT(IN):: ULogCF
      INTEGER, INTENT(IN):: Kit(Dim%Simul, Dim%Kit)
      LOGICAL, INTENT(IN):: FLastPass
      INTEGER, INTENT(IN):: FlagFilt
      LOGICAL, INTENT(IN):: FlagRendProm
      LOGICAL, INTENT(IN):: FRendBdrs
      LOGICAL, INTENT(IN):: FOnePhi
      LOGICAL, INTENT(IN):: FSeparaFCF
      LOGICAL, INTENT(IN):: PDSvFl
      LOGICAL, INTENT(IN):: ErSvFl
      LOGICAL, INTENT(IN):: PsFzFl
      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION, INTENT(IN):: CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION, INTENT(IN):: CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: EstocUBP(Dim%EstocCol, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION, INTENT(IN):: FiltProm(Dim%EmbFilt, Dim%Eta + 1)

      INTEGER, INTENT(IN):: NBloque(Dim%Eta)
      INTEGER, INTENT(IN):: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: EtaDur(Dim%Eta)
      INTEGER, INTENT(IN):: Mes(Dim%Eta)
      INTEGER, INTENT(IN):: Year(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: FactTiempo

      LOGICAL, INTENT(IN):: FScaleQs
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
      DOUBLE PRECISION, INTENT(IN):: ScalePhi
      DOUBLE PRECISION, INTENT(IN):: ScaleVol(Dim%Emb)
      LOGICAL, INTENT(IN):: FVertReb
      DOUBLE PRECISION, INTENT(IN):: EmbVReb(Dim%EmbVReb)
      INTEGER, INTENT(IN):: EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)

      INTEGER, INTENT(IN):: NEmbVReb
      INTEGER, INTENT(IN):: EmbVRebInd(Dim%EmbVReb)

      INTEGER, INTENT(IN):: FConvLaja
      TYPE(PAR_LAJA), INTENT(INOUT):: ParLaja
      TYPE(PAR_LAJAM), INTENT(INOUT):: ParLajaM
      INTEGER, INTENT(IN):: FConvMaule
      TYPE(PAR_MAULE), INTENT(INOUT):: ParMaule
      LOGICAL, INTENT(IN):: FRestRalco
      TYPE(PAR_RALCO), INTENT(IN):: ParRalco
      LOGICAL, INTENT(IN):: FRestGnl
      TYPE(PAR_GNL), INTENT(INOUT):: ParGnl

      LOGICAL, INTENT(IN):: FRestReserva
      TYPE(PAR_RESERVA), INTENT(INOUT):: ParRes

      LOGICAL, INTENT(IN):: FBaterias
      TYPE(PAR_BATERIAS), INTENT(INOUT):: ParBaterias

      LOGICAL, INTENT(IN):: FFiltVar
      INTEGER, INTENT(IN):: FiltVarFilInd(Dim%EmbFilt, Dim%Eta)
      INTEGER, INTENT(IN):: FactDBL

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp(Dim%Simul, Dim%Eta)

      INTEGER, INTENT(OUT):: ArchContP(Dim%Eta, Dim%Simul)
      DOUBLE PRECISION, INTENT(INOUT):: kappa
      INTEGER, INTENT(OUT):: IStat
      DOUBLE PRECISION, INTENT(OUT):: ZSPFAdd

      DOUBLE PRECISION, INTENT(OUT):: MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION, INTENT(OUT):: LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      LOGICAL, INTENT(OUT):: MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)

      DOUBLE PRECISION, INTENT(INOUT):: ZSPFArr

      DOUBLE PRECISION, INTENT(INOUT):: EmbVIni(Dim%Emb, Dim%Simul, Dim%Eta)

!     locals
      DOUBLE PRECISION CenReni(Dim%Cen)

      DOUBLE PRECISION SimDual(MAXVAL(PDNFila))
      DOUBLE PRECISION SimPrimal(MAXVAL(PDNCol))
      INTEGER(C_SIZE_T) lpi
      LOGICAL FFixMaule
      DOUBLE PRECISION VolDefRie
      DOUBLE PRECISION VolFiltInv
      DOUBLE PRECISION VolFiltLaja

      INTEGER IClaseFila(EstocNFila)
      INTEGER IClaseCol(EstocNCol)


      LOGICAL FUndo
      DOUBLE PRECISION Phi
      DOUBLE PRECISION Z

      DOUBLE PRECISION VolIni(Dim%PDLDAcCol)
      DOUBLE PRECISION FiltVals(FiltNCen)

      LOGICAL Infactible

      DOUBLE PRECISION kappai

      INTEGER IPtrD
      INTEGER IPtrP
      INTEGER USimDual
      INTEGER USimPrimal
      LOGICAL FlagFiltProm

      INTEGER IEta
      INTEGER IEtaDest
      INTEGER ICiclo
      LOGICAL SaveNext

      INTEGER IEtaNext
      INTEGER IColAcop
      INTEGER Idx

      INTEGER NumEmb
      CHARACTER*80 filename

      INTEGER I
!
      NumEmb = NCenEmb

      CenReni(1:Dim%Cen) = CenRen(1:Dim%Cen)


      IEta = IEtapa
      SaveNext = .FALSE.
      DO WHILE (.TRUE.)
         lpi = lp(ISimul, IEta)
         VolIni(1:NumEmb) = EmbVIni(1:NumEmb, ISimul, IEta)
         CALL FijaSimu(IEta, ScaleVol,                                  &
     &        VolIni,                                                   &
     &        FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,   &
     &        NumEmb,                                                   &
     &        PDLDAcFilaInd,                                            &
     &        lpi, Dim)
         CALL FijaVar(IEta,                                             &
     &        ScaleObj, ScalePhi,                                       &
     &        PDNCol,                                                   &
     &        ISimul, NSimul, PDNumIte, NEtapa,                         &
     &        FOnePhi, FSeparaFCF, FDepHidEta,                          &
     &        lpi)
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
     &        BloInd(1, IEta), CenRen,                                  &
     &        CenPMin(1, 1, CenManSInd(ISimul)),                        &
     &        CenPMax(1, 1, CenManSInd(ISimul)),                        &
     &        VolIni, CenReni,                                          &
     &        FRestReserva, ParRes,                                     &
     &        lpi, Dim)

         IClaseFila = SimulInd(ISimul, IEta)
         IClaseCol = SimulInd(ISimul, IEta)
         CALL FijaMues(IEta, NBloque(IEta),                             &
     &        CenPMax(1, 1, CenManSInd(ISimul)), CenReni,               &
     &        BloInd(1, IEta), BloDur, FactTiempo, FScaleQs,            &
     &        EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,         &
     &        EstocRHSP, EstocUBP,                                      &
     &        IClaseFila, IClaseCol,                                    &
     &        FBaterias,ParBaterias,                                    &
     &        lpi, Dim)
         CALL Maule1(IEta, NBloque, BloInd, BloDur, CenReni,            &
     &        Cau2Vol, MauleIPar, MauleCPar, MauleLPar, MauleRPar, Mes, &
     &        CauConMauEta, CauRes105Eta,                               &
     &        ISimul, SimulInd, EstocRHSP, VolIni,                      &
     &        VolFiltInv, VolDefRie, Si,                                &
     &        FlagFiltProm, FiltProm, FiltParam, FiltNTramo,            &
     &        lpi, Dim)
         CALL Laja1(IEta, NBloque, BloInd, BloDur, CenReni,             &
     &        Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,          &
     &        Mes,                                                      &
     &        CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,         &
     &        ISimul, SimulInd, EstocRHSP, VolFiltLaja, Si,             &
     &        FlagFiltProm, FiltProm, FiltParam, FiltNTramo,            &
     &        lpi, Dim)
         IF (FConvLaja .EQ. 1) THEN
            CALL FijaLaja(IEta,                                         &
     &           NBloque, BloInd, BloDur, EtaDur, Mes, FactTiempo,      &
     &           EstocRHSP, IClaseFila,                                 &
     &           ParLaja, VolIni, NumEmb,                               &
     &           ISimul,                                                &
     &           lpi, Dim)
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            CALL FijaLajaM(IEta,                                        &
     &           NBloque, BloInd, BloDur, EtaDur, Mes, FactTiempo,      &
     &           EstocRHSP, IClaseFila,                                 &
     &           ParLajaM,ParRes, VolIni, NumEmb, FiltVals,                    &
     &           ISimul,FRestReserva,                                                &
     &           lpi, Dim)
         ENDIF
         IF (FConvMaule .NE. 0) THEN
            CALL FijaMaule(IEta,                                        &
     &           NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,&
     &           EstocRHSP, IClaseFila,                                 &
     &           ParMaule, VolIni, NumEmb, FiltVals,                    &
     &           ISimul,                                                &
     &           lpi, Dim)
         ENDIF
         IF (FRestRalco) THEN
            CALL FijaRalco(IEta, VolIni, NumEmb, ParRalco, lpi)
         ENDIF
         IF (FRestGnl) THEN
            CALL FijaGnl(IEta, ISimul, VolIni, NumEmb, ParGnl, lpi)
         ENDIF


         ICiclo = ArchContP(IEta, ISimul)

         IF ((                                                          &
     &        PDSvFl                                                    &
     &        ) .OR. (                                                  &
     &        (IndEta1Imp .LE. IEta) .AND.                              &
     &        (IndEta2Imp .GE. IEta)                                    &
     &        ) .OR. (                                                  &
     &        (IndIteImp .EQ. PDNumIte + 1) .AND.                       &
     &        (ISimul .EQ. IndSimImp)                                   &
     &        )) THEN
            CALL Salvai(IEta, PDNumIte + 1, ISimul, 0,     &
     &           0, ext//'P', Si, ICiclo, ULog, lpi, .TRUE.)
         ENDIF
         IF (SaveNext) THEN
            CALL Salvai(IEta, PDNumIte + 1, ISimul, 0,     &
     &           0, ext//'M', Si, ICiclo, ULog, lpi, .TRUE.)
            SaveNext = .FALSE.
         ENDIF


         kappai = 0.0d0
         Infactible = .FALSE.

         CALL Resuelve(IStat, lpi, kappai, Dim)
         ArchContP(IEta, ISimul) = ArchContP(IEta, ISimul) + 1
         IF (IStat .EQ. OSI_STAT_OPTIMAL) THEN
            IF (kappai .GT. kappa) THEN
               kappa = kappai
            ENDIF
         ELSEIF (IStat .EQ. OSI_STAT_INFEASIBLE) THEN
            Infactible =  .TRUE.
            IF (FactDBL .NE. 0) THEN
!              Log cortes de factibilidad
               WRITE(ULogCF, '(A, I5, A, I3, A, I3)')                      &
     &              'Problema Infactible Ciclo=',                          &
     &              ICiclo, ', Etapa=', IEta, ', Simul=', ISimul
            ENDIF

            IF (ICiclo + 1 .GT. FactMXC) THEN
               WRITE(6, '(A, I5, A, I5)')                               &
     &              'Problema Infactible Ciclo=',                       &
     &              ICiclo + 1, ' supera limite ', FactMXC
               WRITE(ULog, '(A, I5, A, I5)')                            &
     &              'Problema Infactible Ciclo=',                       &
     &              ICiclo + 1, ' supera limite ', FactMXC
!              Log cortes de factibilidad
               WRITE(ULogCF, '(A, I5, A, I5)')                          &
     &              'Problema Infactible Ciclo=',                       &
     &              ICiclo + 1, ' supera limite ', FactMXC
            ELSE IF (IEta .GT. 1) THEN
!              Tratamos de agregar un corte de infactibilidad y
!              retrocedemos a la etapa anterior
               IF (FactDBL .GT. 1) THEN
                  CALL Salvai(IEta, PDNumIte + 1, ISimul, 0,            &
     &                 0, ext//'N', Si, ICiclo, ULog, lpi, .TRUE.)

                  CALL name4salva(filename, 0, ietapa, isimul,  &
     &                 PDNumIte + 1, PDNumIte + 1, ext//'L', Si, iciclo)
               ENDIF
               IEtaDest = IEta - 1
               CALL  AgrFact( &
     &              FactEPS, FactMLD, FactDBL, FSeparaCFA, FOneFeasRay, &
     &              PDNumIte, NSimul, ISimul, IEta, IEtaDest,           &
     &              NCenEmb,                                            &
     &              FConvLaja, Parlaja, ParLajaM,                       &
     &              FConvMaule, Parmaule,                               &
     &              PDLDAcFilaInd,                                      &
     &              PDLDAcColInd,                                       &
     &              lp, IStat, filename, ULogCF, Dim, UPlane)
               IF (IStat .EQ. 0) THEN
                  IF (FactDBL .NE. 0) THEN
!                    Log cortes de factibilidad
                     WRITE(ULogCF, '(A, I5, A, I3, A, I3)')                &
     &                    'Se agrego corte de factibilidad Ciclo=',        &
     &                    ICiclo, ' EtaDest=', IEtaDest, ' Simul=', ISimul
                  ENDIF
                  IEta = IEta - 1
                  Infactible = .FALSE.
                  IF (FactDBL .GT. 1) THEN
                     SaveNext = .TRUE.
                  ENDIF
                  CYCLE
               ELSE
                  WRITE(ULog, *)  'No hay corte fact, istat=', IStat
                  WRITE(ULogCF, *)  'No hay corte fact, istat=', IStat
               ENDIF
            ENDIF
         ELSE
            IF (ErSvFl) THEN
               CALL Salvai(IEta, PDNumIte + 1, ISimul, 0,               &
     &              0, ext//'E', Si, ICiclo, ULog, lpi, .TRUE.)
            ENDIF
            WRITE(ULog, '(A, I4, A, I3, A, I3, A)')                     &
     &           'faseprim: Error en etapa/simul ', IEta, '/', ISimul,  &
     &           ' en la fase primal con flag ', IStat, '.'
            WRITE(6, *)
            WRITE(6, '(A, A, A)')                                       &
     &           'faseprim: Revisar el archivo ''',                     &
     &           NArcLog, ''''
            IF (PsFzFl) THEN
!     Es posible que el error no sea tan grave...
               IStat = OSI_STAT_OPTIMAL
            ELSE
               STOP 1
            ENDIF
         ENDIF
         IF (Infactible) THEN
            IF (ErSvFl) THEN
               CALL Salvai(IEta, PDNumIte + 1, ISimul, 0,               &
     &              0, ext//'I', Si, ICiclo, ULog, lpi, .TRUE.)
            ENDIF
            WRITE(6, *)
            WRITE(6, '(A, A, A, A)')                                       &
     &           'faseprim: Infactibilidad. ',                             &
     &           'Revisar el archivo ''', NArcLog, ''''
            WRITE(Ulog, '(A, A, A, A)')                                    &
     &           'faseprim: Infactibilidad. ',                             &
     &           'Revisar el archivo ''', NArcLog, ''''
            IF (PsFzFl) THEN
!     Es posible que el error no sea tan grave...
               IStat = OSI_STAT_OPTIMAL
            ELSE
               STOP 1
            ENDIF
         ENDIF
        CALL TrasPrimal(PDNCol(IEta), ScaleObj,                           &
     &        Phi, Z, SimPrimal,                                          &
     &        lpi)
         IF (FLastPass) THEN
            FUndo = .FALSE.
            CALL Maule1a(FFixMaule, FUndo,                                &
     &           IEta, NBloque, ISimul,                                   &
     &           MauleIPar, MauleCPar, MauleRPar, CenReni,                 &
     &           SimPrimal, PDNCol(IEta),                                 &
     &           lpi, Dim)
         ENDIF
         CALL TrasDualSimul(PDNFila(IEta), ScaleObj,                    &
     &        SimDual, lpi)
         CALL Maule2(IEta, NEtapa, NBloque, BloInd, BloDur,             &
     &        Cau2Vol, MauleIPar, MauleCPar, MauleLPar, MauleRPar, Mes, &
     &        Si, ISimul, SimulInd, EstocRHSP, SimPrimal, PDNCol(IEta), &
     &        VolFiltInv, VolDefRie, Dim)
         CALL Laja2(IEta, NEtapa, NBloque, BloInd, BloDur,              &
     &        ScaleVol, CauDemNuReEta,                                  &
     &        Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,          &
     &        Mes, MesBal,                                              &
     &        Si, ISimul, SimulInd, EstocRHSP, SimPrimal, PDNCol(IEta), &
     &        VolFiltLaja, lpi, Dim)
         IF (FLastPass) THEN
            IF (FFixMaule) THEN
               FUndo = .TRUE.
               CALL Maule1a(FFixMaule, FUndo,                           &
     &              IEta, NBloque, ISimul,                              &
     &              MauleIPar, MauleCPar, MauleRPar, CenReni,           &
     &              SimPrimal, PDNCol(IEta),                            &
     &              lpi, Dim)
            ENDIF
         ENDIF

         IF (FLastPass) THEN
            USimPrimal = Kit(ISimul, ISPFA)
            IPtrP = NSimul*SUM(PDNCol(1:IEta-1))                        &
     &           + (ISimul - 1)*PDNCol(IEta) + 1
            CALL RAM2AADd(USimPrimal, IPtrP, SimPrimal,                 &
     &           PDNCol(IEta))
            USimDual = Kit(ISimul, ISDFA)
            IPtrD = NSimul*SUM(PDNFila(1:IEta-1))                       &
     &           + (ISimul - 1)*PDNFila(IEta) + 1
            CALL RAM2AADd(USimDual, IPtrD, SimDual,                     &
     &           PDNFila(IEta))
         ENDIF

         IF (IEta .LT. NEtapa) THEN
            IEtaNext = IEta + 1
            DO IColAcop = 1, NumEmb
               Idx = PDLDAcColInd(IColAcop, IEta)
               EmbVIni(IColAcop, ISimul, IEtaNext) =                    &
     &              ScaleVol(IColAcop) * SimPrimal(Idx)
            ENDDO

            IF (FConvLaja .EQ. 1) THEN
               DO IColAcop = 1, ParLaja%NumColEta
                  Idx = ParLaja%ColIndEta(IColAcop, IEta)
                  IF (ParLaja%VarEtaVol(IColAcop)) THEN
                     ParLaja%VarEtaPrev(IColAcop, ISimul, IEtaNext) =   &
     &                    SimPrimal(Idx) * ParLaja%ScaleVol
                  ELSE
                     ParLaja%VarEtaPrev(IColAcop, ISimul, IEtaNext) =   &
     &                    SimPrimal(Idx)
                  ENDIF
               ENDDO
            ENDIF
            IF (FConvLaja .EQ. 2) THEN
               DO IColAcop = 1, ParLajaM%NumColEta
                  Idx = ParLajaM%ColIndEta(IColAcop, IEta)
                  IF (ParLajaM%VarEtaVol(IColAcop)) THEN
                     ParLajaM%VarEtaPrev(IColAcop, ISimul, IEtaNext) =   &
     &                    SimPrimal(Idx) * ParLajaM%ScaleVol
                  ELSE
                     ParLajaM%VarEtaPrev(IColAcop, ISimul, IEtaNext) =   &
     &                    SimPrimal(Idx)
                  ENDIF
               ENDDO
            ENDIF

            IF (FConvMaule .NE. 0) THEN
               DO IColAcop = 1, ParMaule%NumColEta
                  Idx = ParMaule%ColIndEta(IColAcop, IEta)
                  IF (ParMaule%VarEtaVol(IColAcop)) THEN
                     ParMaule%VarEtaPrev(IColAcop, ISimul, IEtaNext) =  &
     &                    SimPrimal(Idx) * ParMaule%ScaleVol
                  ELSE
                     ParMaule%VarEtaPrev(IColAcop, ISimul, IEtaNext) =  &
     &                    SimPrimal(Idx)
                  ENDIF
               ENDDO
            ENDIF

            IF (FRestGnl) THEN
               Do I = 1, ParGnl%NumTGNL
                  Idx = ParGnl%TGNL(I)%ColIndEta(ParGnl%TGNL(I)%IVGNLF, IEta)
                  ParGnl%TGNL(I)%VolEtaPrev(ISimul, IEtaNext) =   &
     &                 SimPrimal(Idx)
               ENDDO
            ENDIF

         ENDIF

         IF (IEta .LT. IEtapa) THEN
            IEta = IEta + 1
            CYCLE
         ENDIF

!
!     Termino del while loop
!
         IF (IEta .LT. NEtapa) THEN
            ZSPFAdd = Z - Phi
         ELSE
            ZSPFAdd = Z
         ENDIF
         ZSPFArr = ZSPFArr + ZSPFAdd

         EXIT

      ENDDO

      RETURN
      END
